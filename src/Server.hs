-- | "Server" contains functions that serves the Penrose runtime over
--   websockets connection.
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Server where
import           Control.Concurrent            (MVar, forkIOWithUnmask,
                                                modifyMVar, modifyMVar_,
                                                newMVar, readMVar)
import           Control.Exception
import           Control.Monad                 (forM_, forever, void)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson
import           Data.Char                     (isPunctuation, isSpace)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   (mappend)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Tuple.Extra              (fst3)
import           Debug.Trace
import qualified GenOptProblem                 as G
import           GHC.Float                     (float2Double)
import           GHC.Generics
import qualified Network.Socket                as S
import qualified Network.WebSockets            as WS
import           Network.WebSockets.Connection
import qualified Network.WebSockets.Stream     as Stream
import qualified Optimizer                     as O
import           Shapes
import qualified Sugarer
-- (Shape, Value (..), getName,
--                                                 getNum, getX, getY, sampleShapes, setX,
--                                                 setY, toPolymorphics, set, is)
import qualified Style                         as N
import           Substance                     (parseSubstance)
import qualified System.Console.Pretty         as Console
import           System.Console.Pretty         (Color (..), Style (..), bgColor,
                                                color, supportsPretty)
import           System.Random
import           System.Time
import           Utils                         (Autofloat, divLine, fromRight,
                                                r2f, trRaw)

import           Data.UUID
import           Env                           (VarEnv)
import           Dsll                          (parseDsll)
import           GenOptProblem
import           Style
import System.IO (stderr, Handle)
import System.Log.Logger (rootLoggerName, setHandlers, updateGlobalLogger,
                          Priority(..), infoM, debugM,
                          warningM, errorM, setLevel)
import System.Log.Handler.Simple (fileHandler, streamHandler, GenericHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import Text.Show.Pretty
--------------------------------------------------------------------------------
-- Types

data Packet a = Packet {
    typ :: String,
    contents :: a
} deriving Generic
instance (ToJSON a) => ToJSON (Packet a) where
    toJSON Packet{typ=t, contents=c} = object ["type" .= t, "contents" .= c]

data CompilerError = SubError String | StyError String | ElmtError String
    deriving Generic
instance ToJSON CompilerError

-- | TODO
type ClientID = UUID

-- | TODO
type ServerState = [Client]

-- | TODO
type Client = (ClientID, WS.Connection, ClientState)

-- | TODO
data ClientState
    = Editor VarEnv StyProg (Maybe BackendState)
    | Renderer BackendState

-- | TODO
type BackendState = GenOptProblem.State

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

-- | TODO
servePenrose :: VarEnv   -- ^ Element environment
             -> StyProg  -- ^ parsed Style program
             -> String   -- ^ the domain of the server
             -> Int      -- ^ port number of the server
             -> IO ()
servePenrose env sty domain port = do
    initState <- newMVar newServerState
    catch (runServer domain port $ handleClient env sty initState) handler
    where
        handler :: ErrorCall -> IO ()
        handler e = putStrLn "Internal server Error"


handleClient :: VarEnv           -- ^ Element environment
             -> StyProg          -- ^ parsed Style program
             -> MVar ServerState -- ^ current list of clients
             -> WS.ServerApp
handleClient env styProg state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30 -- To keep the connection alive
    clients  <- readMVar state
    clientID <- newUUID
    let clientState = Editor env styProg Nothing
    let client      = (clientID, conn, clientState)
    let logPath = "/tmp/penrose-" ++ idString client ++ ".log"
    myStreamHandler <- streamHandler stderr INFO
    myFileHandler <- fileHandler logPath INFO
    let myFileHandler' = withFormatter myFileHandler
    let myStreamHandler' = withColoredFormatter myStreamHandler
    updateGlobalLogger rootLoggerName (setHandlers [myStreamHandler', myFileHandler'])
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    flip finally (disconnect client) $ do
        modifyMVar_ state $ \s -> do
            let s' = addClient client s
            return s'
        logInfo client $ "Client connected " ++ toString clientID
        -- start an editor session
        waitSubstance client
    where disconnect client= do
              -- Remove client
              modifyMVar_ state $ \s ->
                return $ removeClient client s
              logInfo client (idString client ++ " disconnected")

--------------------------------------------------------------------------------
-- Client-level functions

updateState :: ClientState -> BackendState -> ClientState
updateState (Renderer s) s'     = Renderer s'
updateState (Editor e sty s) s' = Editor e sty $ Just s'

getBackendState :: ClientState -> BackendState
getBackendState (Renderer s) = s
getBackendState (Editor _ _ (Just s)) = s
getBackendState (Editor _ _ Nothing) = error "Server error: Backend state has not been initialized yet."

data Feedback
    = Cmd Command
    | Drag DragEvent
    | Update UpdateShapes
    | Edit SubstanceEdit
    | Recompile RecompileDomain
    deriving (Generic)

data Command = Command { command :: String }
     deriving (Show, Generic)

data DragEvent = DragEvent { name :: String,
                             xm   :: Float,
                             ym   :: Float }
     deriving (Show, Generic)

data SubstanceEdit = SubstanceEdit { program :: String, enableAutostep :: Bool }
     deriving (Show, Generic)

data RecompileDomain = RecompileDomain { element :: String, style :: String }
     deriving (Show, Generic)

data UpdateShapes = UpdateShapes { shapes :: [Shape Double] }
    deriving (Show, Generic)

data Frame = Frame { flag   :: String,
                     shapes :: [Shape Double],
                     ordering :: [String]
                   } deriving (Show, Generic)

instance FromJSON Feedback
instance FromJSON Command
instance FromJSON DragEvent
instance FromJSON UpdateShapes
instance FromJSON SubstanceEdit
instance FromJSON RecompileDomain
instance ToJSON Frame


-- | 'serveRenderer' is the top-level function that "Main" uses to start serving
--   the Penrose Runtime.
serveRenderer :: String  -- the domain of the server
              -> Int  -- port number of the server
              -> BackendState  -- initial state of Penrose Runtime
              -> IO ()
serveRenderer domain port initState = do
     putStrLn "Starting Server..."
     let s = Renderer initState
     catch (runServer domain port $ renderer s) handler
     where
         handler :: ErrorCall -> IO ()
         handler _ = putStrLn "Server Error"

-- | 'serveREditor' starts the Penrose server without compiling a Substance program. The server enters the editor mode and compile user's input Substance programs dynamically
serveEditor :: String   -- ^ the domain of the server
            -> Int      -- ^ port number of the server
            -> VarEnv   -- ^ Element environment
            -> StyProg  -- ^ parsed Style program
            -> IO ()
serveEditor domain port env styProg = do
     putStrLn "Starting Server..."
     let initState = Editor env styProg Nothing
     catch (runServer domain port $ editor initState) handler
     where
         handler :: ErrorCall -> IO ()
         handler e = putStrLn "Server Error"

editor, renderer :: ClientState -> WS.ServerApp
editor clientState@Editor {} pending = do
    conn <- WS.acceptRequest pending
    clientID <- newUUID
    let client = (clientID, conn, clientState)
    WS.forkPingThread conn 30 -- To keep the connection alive
    waitSubstance client
renderer (Renderer s) pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30 -- To keep the connection alive
    wsSendFrame conn
        Frame {
            flag = "initial",
            ordering = shapeOrdering s,
            shapes = shapesr s :: [Shape Double]
        }
    clientID <- newUUID
    let clientState = Renderer $ O.step s
    let client = (clientID, conn, clientState)
    waitUpdate client

loop :: Client -> IO ()
loop client@(clientID, conn, clientState)
    | optStatus (paramsr s) == EPConverged = do
        logInfo client "Optimization completed."
        logInfo client ("Current weight: " ++ show (weight (paramsr s)))
        wsSendFrame conn
            Frame {
                flag = "final",
                ordering = shapeOrdering s,
                shapes = shapesr s :: [Shape Double]
            }
        processCommand client
    | autostep s = stepAndSend client
    | otherwise = processCommand client
    where s = getBackendState clientState

-- | In editor mode, the server first waits for a well-formed Substance program
-- before accepting any other kinds of commands. The default action on other
-- commands is to continue waiting without crashing
waitSubstance :: Client -> IO ()
waitSubstance client@(clientID, conn, clientState) = do
    infoM (toString clientID) "Waiting for Substance program..."
    msg_json <- WS.receiveData conn
    case decode msg_json of
        Just e -> case e of
            Edit (SubstanceEdit subProg auto) -> substanceEdit subProg auto client
            Recompile (RecompileDomain element style) -> recompileDomain element style client
            _                             -> continue
        Nothing -> continue
    where continue = do
              warningM (toString clientID) "Invalid command. Returning to wait for Substance program"
              waitSubstance client

-- } COMBAK: abstract this logic out to `wait`
waitUpdate :: Client -> IO ()
waitUpdate client@(clientID, conn, clientState) = do
    logInfo client "Waiting for label dimension update"
    msg_json <- WS.receiveData conn
    case decode msg_json of
        Just e -> case e of
            Update (UpdateShapes shapes) -> updateShapes shapes client
            _                            -> continue
        Nothing -> continue
    where continue = do
            warningM (toString clientID) "Invalid command. Returning to wait for label update."
            waitUpdate client

substanceError, elementError, styleError :: Client -> ErrorCall -> IO ()
substanceError client@(_, conn, _) e = do
     logError client $ "Substance compiler error: " ++ show e
     wsSendPacket conn Packet { typ = "error", contents = SubError $ show e}
     waitSubstance client
elementError client@(_, conn, _) e = do
     logError client $ "Element parser error: " ++ show e
     wsSendPacket conn Packet { typ = "error", contents = SubError $ show e}
     waitSubstance client
styleError client@(_, conn, _) e = do
     logError client $ "Style parser error: " ++ show e
     wsSendPacket conn Packet { typ = "error", contents = SubError $ show e}
     waitSubstance client

-- -- TODO: this match might be redundant, but not sure why the linter warns that.
-- substanceError client s _ = do
--     putStrLn "Substance compiler error: Unknown error."
--     waitSubstance c s


-- COMBAK: this function should be updated to remove
processCommand :: Client -> IO ()
processCommand client@(clientID, conn, s) = do
    logInfo client "Waiting for Commands"
    msg_json <- WS.receiveData conn
    logInfo client $ "Messege received from frontend: \n" ++ show msg_json
    case decode msg_json of
        Just e -> case e of
            Cmd (Command cmd)            -> executeCommand cmd client
            Drag (DragEvent name xm ym)  -> dragUpdate name xm ym client
            Edit (SubstanceEdit subProg auto) -> substanceEdit subProg auto client
            Update (UpdateShapes shapes) -> updateShapes shapes client
            Recompile (RecompileDomain element style) -> recompileDomain element style client
        Nothing -> logError client "Error reading JSON"
        -- TODO: might need to return to `loop`

recompileDomain :: String -> String -> Client -> IO ()
recompileDomain _ _ client@(_, conn, Renderer s) = do
    logError client "Cannot change domains in Renderer mode."
    loop client
recompileDomain element style client@(clientID, conn, Editor {}) = do
    logInfo client "Switching to another domain..."
    logInfo client $ "Element program received: " ++ element
    logInfo client $ "Style program received: " ++ style

    elementRes <- try $ parseDsll "" element
    case elementRes of
        Right elementEnv -> do
            styRes <- try $ parseStyle "" style elementEnv
            case styRes of
                Right styProg -> do
                    logDebug client ("Style AST:\n" ++ ppShow styProg)
                    let client = (clientID, conn, Editor elementEnv styProg Nothing)
                    waitSubstance client
                Left err -> styleError client err
        Left err -> elementError client err

substanceEdit :: String -> Bool -> Client -> IO ()
substanceEdit subIn _ client@(_, _, Renderer _) =
    logError client "Server Error: the Substance program cannot be updated when the server is in Renderer mode."
substanceEdit subIn auto client@(clientID, conn, Editor env styProg s) = do
    logInfo client $ "Substance program received: " ++ subIn
    subRes <- try (parseSubstance "" (Sugarer.sugarStmts subIn env) env)
    case subRes of
        Right subOut -> do
            logDebug client $ show subOut
            styRes <- try (compileStyle styProg subOut)
            case styRes of
                Right newState -> do
                    wsSendFrame conn Frame {
                        flag = "initial",
                        ordering = shapeOrdering newState,
                        shapes = shapesr newState :: [Shape Double]
                    }
                    waitUpdate (clientID, conn, Editor env styProg $ Just newState { G.autostep = auto })
                Left styError -> substanceError client styError
        Left subError -> substanceError client subError

updateShapes :: [Shape Double] -> Client -> IO ()
updateShapes newShapes client@(clientID, conn, clientState) =
    let polyShapes = toPolymorphics newShapes
        uninitVals = map G.toTagExpr $ G.shapes2vals polyShapes $ G.uninitializedPaths s
        trans' = G.insertPaths (G.uninitializedPaths s) uninitVals (G.transr s)
        newObjFn = G.genObjfn trans' (G.objFns s) (G.constrFns s) (G.varyingPaths s)
        varyMapNew = G.mkVaryMap (G.varyingPaths s) (G.varyingState s)
        news = s {
            G.shapesr = polyShapes,
            G.varyingState = G.shapes2floats polyShapes varyMapNew $ G.varyingPaths s,
            G.transr = trans',
            G.paramsr = (G.paramsr s) { G.weight = G.initWeight, G.optStatus = G.NewIter, G.overallObjFn = newObjFn }}
        nextClientS = updateState clientState news
        client' = (clientID, conn, nextClientS)
    in if autostep s
        then stepAndSend client'
        else loop client'
    where s = getBackendState clientState

dragUpdate :: String -> Float -> Float -> Client -> IO ()
dragUpdate name xm ym client@(clientID, conn, clientState) =
    let (xm', ym') = (r2f xm, r2f ym)
        newShapes  = map (\shape ->
            if getName shape == name
                then dragShape shape xm' ym'
                else shape)
            (G.shapesr s)
        varyMapNew = G.mkVaryMap (G.varyingPaths s) (G.varyingState s)
        news = s { G.shapesr = newShapes,
                   G.varyingState = G.shapes2floats newShapes varyMapNew $ G.varyingPaths s,
                   G.paramsr = (G.paramsr s) { G.weight = G.initWeight, G.optStatus = G.NewIter }}
        nextClientS = updateState clientState news
        client' = (clientID, conn, nextClientS)
    in if autostep s
        then stepAndSend client'
        else loop client'
    where s = getBackendState clientState

dragShape :: Autofloat a => Shape a -> a -> a -> Shape a
dragShape shape dx dy
    | shape `is` "Line" =
        trRaw "here"  $ move "startX" dx $
        move "startY" dy $
        move "endX"   dx $
        move "endY"   dy shape
    | shape `is` "Image" =
        move "centerX" dx $
        move "centerY" dy shape
    | shape `is` "Arrow" =
        move "startX" dx $
        move "startY" dy $
        move "endX"   dx $
        move "endY"   dy shape
    | otherwise = setX (FloatV (dx + getX shape)) $ setY (FloatV (dy + getY shape)) shape

move :: Autofloat a => PropID -> a -> Shape a -> Shape a
move prop dd s = set s prop (FloatV (dd + getNum s prop))

executeCommand :: String -> Client -> IO ()
executeCommand cmd client@(clientID, conn, clientState)
    | cmd == "resample" = resampleAndSend client
    | cmd == "step"     = stepAndSend client
    | cmd == "autostep" =
        let os  = getBackendState clientState
            os' = os { autostep = not $ autostep os }
        in loop (clientID, conn, updateState clientState os')
    | otherwise         = logError client ("Can't recognize command " ++ cmd)

resampleAndSend, stepAndSend :: Client -> IO ()
resampleAndSend client@(clientID, conn, clientState) = do
    -- Sample several states and choose the one with lowest energy
    let news = G.resampleBest G.numStateSamples s
    let (newShapes, _, _) = evalTranslation news
    wsSendFrame conn
        Frame {
            flag = "initial",
            ordering = shapeOrdering news,
            shapes = newShapes
        }
    let nextClientS = updateState clientState news
    let client' = (clientID, conn, nextClientS)
    -- NOTE: could have called `loop` here, but this would result in a race condition between autostep and updateShapes somehow. Therefore, we explicitly transition to waiting for an update on label sizes whenever resampled.
    waitUpdate client'
    where s = getBackendState clientState

stepAndSend client@(clientID, conn, clientState) = do
    let s = getBackendState clientState
    let nexts = O.step s
    -- wsSendJSONList conn (shapesr nexts :: [Shape Double])
    wsSendFrame conn
        Frame {
            flag = "running",
            ordering = shapeOrdering s,
            shapes = shapesr s :: [Shape Double]
        }
    -- loop conn (trRaw "state:" nexts)
    loop (clientID, conn, updateState clientState nexts)

--------------------------------------------------------------------------------
-- Logger

withColoredFormatter, withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    where formatter = simpleLogFormatter ("[$time $loggername $prio]" ++ "\n$msg")
withColoredFormatter handler = setFormatter handler formatter
    where formatter = simpleLogFormatter (bgColor Red $ Console.style Bold "[$time $loggername $prio]" ++ "\n$msg")

logDebug, logInfo, logError :: Client -> String -> IO ()
logDebug client = debugM (idString client)
logInfo  client = infoM  (idString client)
logError client = errorM (idString client)

--------------------------------------------------------------------------------
-- WebSocket utils

-- TODO use the more generic wsSendJSON?
wsSendShapes :: WS.Connection -> [Shape Double] -> IO ()
wsSendShapes conn shapes = WS.sendTextData conn $ encode packet
    where packet = Packet { typ = "shapes", contents = shapes }

wsSendFrame :: WS.Connection -> Frame -> IO ()
wsSendFrame conn frame = WS.sendTextData conn $ encode packet
    where packet = Packet { typ = "shapes", contents = frame }

wsSendPacket :: ToJSON a => WS.Connection -> Packet a -> IO ()
wsSendPacket conn packet = WS.sendTextData conn $ encode packet


-- | This 'runServer' is exactly the same as the one in "Network.WebSocket". Duplicated for calling a customized version of 'runServerWith' with error messages enabled.
runServer :: String     -- ^ Address to bind
          -> Int        -- ^ Port to listen on
          -> WS.ServerApp  -- ^ Application
          -> IO ()      -- ^ Never returns
runServer host port app = runServerWith host port WS.defaultConnectionOptions app

-- | A version of 'runServer' which allows you to customize some options.
runServerWith :: String -> Int -> WS.ConnectionOptions -> WS.ServerApp -> IO ()
runServerWith host port opts app = S.withSocketsDo $
  bracket
  (WS.makeListenSocket host port)
  S.close
  (\sock ->
    mask_ $ forever $ do
      allowInterrupt
      (conn, _) <- S.accept sock
      void $ forkIOWithUnmask $ \unmask ->
        finally (unmask $ runApp conn opts app) (S.close conn)
    )

runApp :: S.Socket
       -> WS.ConnectionOptions
       -> WS.ServerApp
       -> IO ()
runApp socket opts app = do
       sock <- WS.makePendingConnection socket opts
       app sock
