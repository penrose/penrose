-- | "Server" contains functions that serves the Penrose runtime over
--   websockets connection.
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# OPTIONS_HADDOCK prune #-}

module Penrose.Server where
  -- ( serveEditor
  -- , serveRenderer
  -- )
  -- where

import           Control.Concurrent        (MVar, forkIOWithUnmask, modifyMVar_,
                                            newMVar, readMVar)
import           Control.Exception
import           Control.Monad             (forever, void)
import           Data.Aeson
import           Data.Tuple.Extra          (fst3)
import           Data.UUID
import           GHC.Generics
import qualified Network.Socket            as S
import qualified Network.WebSockets        as WS
import           Penrose.API
import           Penrose.Env               (VarEnv)
import           Penrose.GenOptProblem     as GenOptProblem
import           Penrose.Serializer
import           Penrose.Style
import           System.Console.Pretty     (Color (..), Style (..), bgColor)
import qualified System.Console.Pretty     as Console
import           System.Exit               (exitFailure)
import           System.IO                 (Handle, stderr)
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (GenericHandler (..), fileHandler,
                                            streamHandler)
import           System.Log.Logger         (Priority (..), debugM, errorM,
                                            infoM, rootLoggerName, setHandlers,
                                            setLevel, updateGlobalLogger,
                                            warningM)
import           System.Random

--------------------------------------------------------------------------------
-- Types
-- | Each client has a unique ID
type ClientID = UUID

-- | The server maintains a list of clients
type ServerState = [Client]

-- | A client has an ID, a connection handle, and can optionally be stateful
type Client = (ClientID, WS.Connection, ClientState)

-- | Clients can either be stateless or stateful
data ClientState
  = CachedState BackendState -- ^ unused option that caches a state in the client
  | Stateless

type BackendState = GenOptProblem.State

--------------------------------------------------------------------------------
-- RESTful server
type Session = String

processRequests :: Client -> IO ()
processRequests client@(_, conn, _) = do
  logDebug client "Waiting for Commands"
  msg_json <- WS.receiveData conn
  logDebug client $ "Message received from frontend: \n" ++ show msg_json
  case eitherDecode msg_json of
    Right Request {requestSession = session, requestCall = call} ->
      case call of
        Step steps s -> sendSafe session "state" $ step s steps
        Resample samples s -> sendSafe session "state" $ resample s samples
        StepUntilConvergence s ->
          sendSafe session "state" $ stepUntilConvergence s
        CompileTrio sub sty elm ->
          sendSafe session "compilerOutput" $ compileTrio sub sty elm
        ReconcileNext s sub sty elm ->
          sendSafe session "compilerOutput" $ reconcileNext s sub sty elm
        GetEnv sub elm -> sendSafe session "varEnv" $ getEnv sub elm
        EnergyValues s -> send session "energies" $ energyValues s
        GetVersion -> send session "version" getVersion
    Left err -> do
      logError client $ "Error reading JSON: " ++ err
      processRequests client
  logDebug client "Messege received and decoded successfully."
  processRequests client
  where
    send session flag content =
      sendPacket
        conn
        Packet
        {packetType = flag, packetContents = content, packetSession = session}
    sendSafe session flag output =
      case output of
        Right res ->
          sendPacket
            conn
            Packet
            {packetType = flag, packetContents = res, packetSession = session}
        Left err ->
          sendPacket
            conn
            Packet
            { packetType = "error"
            , packetContents = err
            , packetSession = session
            }

--------------------------------------------------------------------------------
-- Server-level functions
newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst3 client) . fst3)

newUUID :: IO UUID
newUUID = randomIO

idString :: Client -> String
idString = toString . fst3

prettyAddress :: String -> Int -> String
prettyAddress domain port = "ws://" ++ domain ++ ":" ++ show port ++ "/"

-- | 'serveEditor' starts the Penrose server in the editor mode and compile user's input programs dynamically
serveEditor ::
     String -- ^ the domain of the server
  -> Int -- ^ port number of the server
  -> Bool -- ^ verbosity option
  -> IO ()
serveEditor domain port isVerbose = do
  initState <- newMVar newServerState
  putStrLn $ "Penrose editor server started on " ++ prettyAddress domain port
  putStrLn "Waiting for clients to connect..."
  catch (runServer domain port $ handleClient initState isVerbose) handler
  where
    handler :: ErrorCall -> IO ()
    handler e = putStrLn "Internal server Error"

handleClient ::
     MVar ServerState -- ^ current list of clients
  -> Bool -- ^ verbosity option
  -> WS.ServerApp
handleClient state isVerbose pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30 -- To keep the connection alive
  clients <- readMVar state
  clientID <- newUUID
  let client = (clientID, conn, Stateless)
  let logLevel =
        if isVerbose
          then DEBUG
          else INFO
  myStreamHandler <- fmap withColoredFormatter $ streamHandler stderr logLevel
  updateGlobalLogger rootLoggerName (setHandlers [myStreamHandler])
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  flip finally (disconnect client) $ do
    modifyMVar_ state $ \s -> do
      let s' = addClient client s
      return s'
    logInfo client $ "Client connected " ++ toString clientID
    -- start an editor session
    sendPacket
      conn
      Packet
      { packetType = "connection"
      , packetContents = String "connected"
      , packetSession = Nothing
      }
    processRequests client
  where
    disconnect client
      -- Remove client
     = do
      modifyMVar_ state $ \s -> return $ removeClient client s
      logInfo client (idString client ++ " disconnected")

--   let logPath = "/var/log/penrose-" ++ idString client ++ ".log"
--   myFileHandler <- fmap withFormatter $ fileHandler logPath logLevel
--   updateGlobalLogger
--     rootLoggerName
--     (setHandlers [myStreamHandler, myFileHandler])
--------------------------------------------------------------------------------
-- Client-level functions
-- | 'serveRenderer' is the top-level function that "Main" uses to start serving
--   the Penrose Runtime.
serveRenderer ::
     String -- the domain of the server
  -> Int -- port number of the server
  -> BackendState -- initial state of Penrose Runtime
  -> IO ()
serveRenderer domain port initState = do
  putStrLn $ "Penrose renderer server started on " ++ prettyAddress domain port
  putStrLn "Waiting for the frontend UI to connect..."
  catch (runServer domain port $ renderer initState) handler
  where
    handler :: ErrorCall -> IO ()
    handler _ = putStrLn "Server Error"

renderer :: BackendState -> WS.ServerApp
renderer s pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30 -- To keep the connection alive
  clientID <- newUUID
  let client = (clientID, conn, Stateless)
    -- send the initial state to the frontend renderer first
  sendPacket
    conn
    Packet {packetType = "state", packetContents = s, packetSession = Nothing}
  processRequests client

--------------------------------------------------------------------------------
-- Logger
withColoredFormatter, withFormatter ::
     GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
  where
    formatter = simpleLogFormatter ("[$time $loggername $prio]" ++ "\n$msg")

withColoredFormatter handler = setFormatter handler coloredLogFormatter

coloredLogFormatter :: LogFormatter a
coloredLogFormatter _h (prio, msg) loggername
  | prio == DEBUG = formatter Blue
  | prio == INFO = formatter Green
  | prio == ERROR = formatter Red
  where
    formatter color =
      simpleLogFormatter
        (bgColor color $
         Console.style Bold "[$time $loggername $prio]" ++ "\n$msg")
        _h
        (prio, msg)
        loggername

logDebug, logInfo, logError :: Client -> String -> IO ()
logDebug client = debugM (idString client)

logInfo client = infoM (idString client)

logError client = errorM (idString client)

--------------------------------------------------------------------------------
-- WebSocket utils
sendPacket :: ToJSON a => WS.Connection -> Packet a -> IO ()
sendPacket conn packet = WS.sendTextData conn $ encode packet

-- | This 'runServer' is exactly the same as the one in "Network.WebSocket". Duplicated for calling a customized version of 'runServerWith' with error messages enabled.
runServer ::
     String -- ^ Address to bind
  -> Int -- ^ Port to listen on
  -> WS.ServerApp -- ^ Application
  -> IO () -- ^ Never returns
runServer host port app = runServerWith host port options app
  where
    options =
      WS.defaultConnectionOptions
      { WS.connectionCompressionOptions =
          WS.PermessageDeflateCompression WS.defaultPermessageDeflate
      }

-- | A version of 'runServer' which allows you to customize some options.
runServerWith :: String -> Int -> WS.ConnectionOptions -> WS.ServerApp -> IO ()
runServerWith host port opts app =
  S.withSocketsDo $
  bracket
    (WS.makeListenSocket host port)
    S.close
    (\sock ->
       mask_ $
       forever $ do
         allowInterrupt
         (conn, _) <- S.accept sock
         void $
           forkIOWithUnmask $ \unmask ->
             finally (unmask $ runApp conn opts app) (S.close conn))

runApp :: S.Socket -> WS.ConnectionOptions -> WS.ServerApp -> IO ()
runApp socket opts app = do
  sock <- WS.makePendingConnection socket opts
  app sock
