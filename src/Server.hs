-- | "Server" contains functions that serves the Penrose runtime over
--   websockets connection.
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Server where

import           Control.Concurrent        (MVar, forkIOWithUnmask, modifyMVar_,
                                            newMVar, readMVar)
import           Control.Exception
import           Control.Monad             (forever, void)
import           Data.Aeson
import           Data.Tuple.Extra          (fst3)
import           Data.UUID
import           Env                       (VarEnv)
import           GenOptProblem
import           GHC.Generics
import           Interface
import qualified Network.Socket            as S
import qualified Network.WebSockets        as WS
import           Style
import           System.Console.Pretty     (Color (..), Style (..), bgColor)
import qualified System.Console.Pretty     as Console
import           System.IO                 (Handle, stderr)
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (GenericHandler (..), fileHandler,
                                            streamHandler)
import           System.Log.Logger         (Priority (..), debugM, errorM,
                                            infoM, rootLoggerName,
                                            setHandlers, setLevel,
                                            updateGlobalLogger, warningM)
import           System.Random

--------------------------------------------------------------------------------
-- Types
data Packet a = Packet
  { typ      :: String
  , contents :: a
  } deriving (Generic)

instance (ToJSON a) => ToJSON (Packet a) where
  toJSON Packet {typ = t, contents = c} = object ["type" .= t, "contents" .= c]

-- | TODO
type ClientID = UUID

-- | TODO
type ServerState = [Client]

-- | TODO
type Client = (ClientID, WS.Connection, ClientState)

-- | TODO
data ClientState
  = Editor VarEnv
           StyProg
           (Maybe BackendState) -- TODO: no longer used, remove
  | Renderer BackendState -- TODO: no longer used, remove
  | Stateless

-- | TODO
type BackendState = GenOptProblem.State

--------------------------------------------------------------------------------
-- RESTful server
data Request
  = Step Int
         State
  | Resample Int
             State
  | StepUntilConvergence State
  | CompileTrio String
                String
                String
  | GetEnv String
           String
  deriving (Generic)

instance FromJSON Request

instance ToJSON Request

processRequests :: Client -> IO ()
processRequests client@(_, conn, _) = do
  logDebug client "Waiting for Commands"
  msg_json <- WS.receiveData conn
  logDebug client $ "Messege received from frontend: \n" ++ show msg_json
  case decode msg_json of
    Just e ->
      case e of
        Step steps s -> sendSafe "state" $ Interface.step s steps
        Resample samples s -> sendSafe "state" $ Interface.resample s samples
        StepUntilConvergence s ->
          sendSafe "state" $ Interface.stepUntilConvergence s
        CompileTrio sub sty elm ->
          sendSafe "compilerOutput" $ Interface.compileTrio sub sty elm
        GetEnv sub elm -> sendSafe "varEnv" $ Interface.getEnv sub elm
    Nothing -> do
      logError client "Error reading JSON"
      processRequests client
  logDebug client $ "Messege received and decoded successfully."
  processRequests client
  where
    sendSafe :: (ToJSON a, ToJSON b) => String -> Either a b -> IO ()
    sendSafe flag res =
      case res of
        Right state -> wsSendPacket conn Packet {typ = flag, contents = state}
        Left error -> wsSendPacket conn Packet {typ = "error", contents = error}

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
  let logLevel = if isVerbose then DEBUG else INFO
  myStreamHandler <- fmap withColoredFormatter $ streamHandler stderr logLevel
  updateGlobalLogger rootLoggerName (setHandlers [myStreamHandler])

--   let logPath = "/var/log/penrose-" ++ idString client ++ ".log"
--   myFileHandler <- fmap withFormatter $ fileHandler logPath logLevel
--   updateGlobalLogger
--     rootLoggerName
--     (setHandlers [myStreamHandler, myFileHandler])

  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  flip finally (disconnect client) $ do
    modifyMVar_ state $ \s -> do
      let s' = addClient client s
      return s'
    logInfo client $ "Client connected " ++ toString clientID
    -- start an editor session
    processRequests client
  where
    disconnect client
     = do
      -- Remove client
      modifyMVar_ state $ \s -> return $ removeClient client s
      logInfo client (idString client ++ " disconnected")

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
  let s = Renderer initState
  catch (runServer domain port $ renderer s) handler
  where
    handler :: ErrorCall -> IO ()
    handler _ = putStrLn "Server Error"

renderer :: ClientState -> WS.ServerApp
renderer (Renderer s) pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30 -- To keep the connection alive
  clientID <- newUUID
  let client = (clientID, conn, Stateless)
    -- send the initial state to the frontend renderer first
  wsSendPacket conn Packet {typ = "state", contents = s}
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
wsSendPacket :: ToJSON a => WS.Connection -> Packet a -> IO ()
wsSendPacket conn packet = WS.sendTextData conn $ encode packet

-- | This 'runServer' is exactly the same as the one in "Network.WebSocket". Duplicated for calling a customized version of 'runServerWith' with error messages enabled.
runServer ::
     String -- ^ Address to bind
  -> Int -- ^ Port to listen on
  -> WS.ServerApp -- ^ Application
  -> IO () -- ^ Never returns
runServer host port app =
  runServerWith host port WS.defaultConnectionOptions app

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
--------------------------------------------------------------------------------
-- Old server code for reference
-- -- TODO use the more generic wsSendJSON?
-- wsSendShapes :: WS.Connection -> [Shape Double] -> IO ()
-- wsSendShapes conn shapes = WS.sendTextData conn $ encode packet
--     where packet = Packet { typ = "shapes", contents = shapes }
-- wsSendFrame :: WS.Connection -> Frame -> IO ()
-- wsSendFrame conn frame = WS.sendTextData conn $ encode packet
--     where packet = Packet { typ = "shapes", contents = frame }
-- updateState :: ClientState -> BackendState -> ClientState
-- updateState (Renderer s) s'     = Renderer s'
-- updateState (Editor e sty s) s' = Editor e sty $ Just s'
-- getBackendState :: ClientState -> BackendState
-- getBackendState (Renderer s) = s
-- getBackendState (Editor _ _ (Just s)) = s
-- getBackendState (Editor _ _ Nothing) = error "Server error: Backend state has not been initialized yet."
-- data Feedback
--     = Cmd Command
--     | Drag DragEvent
--     | Update UpdateShapes
--     | Edit SubstanceEdit
--     | Recompile RecompileDomain
--     deriving (Generic)
-- data Command = Command { command :: String }
--      deriving (Show, Generic)
-- data DragEvent = DragEvent { name :: String,
--                              xm   :: Float,
--                              ym   :: Float }
--      deriving (Show, Generic)
-- data SubstanceEdit = SubstanceEdit { program :: String, enableAutostep :: Bool }
--      deriving (Show, Generic)
-- data RecompileDomain = RecompileDomain { element :: String, style :: String }
--      deriving (Show, Generic)
-- data UpdateShapes = UpdateShapes { shapes :: [Shape Double] }
--     deriving (Show, Generic)
-- data Frame = Frame { flag   :: String,
--                      shapes :: [Shape Double],
--                      ordering :: [String]
--                    } deriving (Show, Generic)
-- instance FromJSON Feedback
-- instance FromJSON Command
-- instance FromJSON DragEvent
-- instance FromJSON UpdateShapes
-- instance FromJSON SubstanceEdit
-- instance FromJSON RecompileDomain
-- instance ToJSON Frame
-- loop :: Client -> IO ()
-- loop client@(clientID, conn, clientState)
--     | optStatus (paramsr s) == EPConverged = do
--         logInfo client "Optimization completed."
--         logInfo client ("Current weight: " ++ show (weight (paramsr s)))
--         wsSendFrame conn
--             Frame {
--                 flag = "final",
--                 ordering = shapeOrdering s,
--                 shapes = shapesr s :: [Shape Double]
--             }
--         processCommand client
--     | autostep s = stepAndSend client
--     | otherwise = processCommand client
--     where s = getBackendState clientState
-- -- | In editor mode, the server first waits for a well-formed Substance program
-- -- before accepting any other kinds of commands. The default action on other
-- -- commands is to continue waiting without crashing
-- waitSubstance :: Client -> IO ()
-- waitSubstance client@(clientID, conn, clientState) = do
--     infoM (toString clientID) "Waiting for Substance program..."
--     msg_json <- WS.receiveData conn
--     case decode msg_json of
--         Just e -> case e of
--             Edit (SubstanceEdit subProg auto) -> substanceEdit subProg auto client
--             Recompile (RecompileDomain element style) -> recompileDomain element style client
--             _                             -> continue
--         Nothing -> continue
--     where continue = do
--               warningM (toString clientID) "Invalid command. Returning to wait for Substance program"
--               waitSubstance client
-- -- } COMBAK: abstract this logic out to `wait`
-- waitUpdate :: Client -> IO ()
-- waitUpdate client@(clientID, conn, clientState) = do
--     logInfo client "Waiting for image/label dimension update"
--     msg_json <- WS.receiveData conn
--     case decode msg_json of
--         Just e -> case e of
--             Update (UpdateShapes shapes) -> updateShapes shapes client
--             _                            -> continue
--         Nothing -> continue
--     where continue = do
--             warningM (toString clientID) "Invalid command. Returning to wait for image/label update."
--             waitUpdate client
-- substanceError, elementError, styleError :: Client -> CompilerError -> IO ()
-- substanceError client@(_, conn, _) e = do
--      logError client $ "Substance compiler error: " ++ show e
--      wsSendPacket conn Packet { typ = "error", contents = e}
--      waitSubstance client
-- elementError client@(_, conn, _) e = do
--      logError client $ "Element parser error: " ++ show e
--      wsSendPacket conn Packet { typ = "error", contents = e}
--      waitSubstance client
-- styleError client@(_, conn, _) e = do
--      logError client $ "Style parser error: " ++ show e
--      wsSendPacket conn Packet { typ = "error", contents = e}
--      waitSubstance client
-- styleRuntimeError :: Client -> ErrorCall -> IO ()
-- styleRuntimeError client@(_, conn, _) e = do
--      logError client $ "Style runtime error: " ++ show e
--      wsSendPacket conn Packet { typ = "error", contents = show e}
--      waitSubstance client
-- -- -- TODO: this match might be redundant, but not sure why the linter warns that.
-- -- substanceError client s _ = do
-- --     putStrLn "Substance compiler error: Unknown error."
-- --     waitSubstance c s
-- -- COMBAK: this function should be updated to remove
-- processCommand :: Client -> IO ()
-- processCommand client@(clientID, conn, s) = do
--     logInfo client "Waiting for Commands"
--     msg_json <- WS.receiveData conn
--     logInfo client $ "Messege received from frontend: \n" ++ show msg_json
--     case decode msg_json of
--         Just e -> case e of
--             Cmd (Command cmd)            -> executeCommand cmd client
--             Drag (DragEvent name xm ym)  -> dragUpdate name xm ym client
--             Edit (SubstanceEdit subProg auto) -> substanceEdit subProg auto client
--             Update (UpdateShapes shapes) -> updateShapes shapes client
--             Recompile (RecompileDomain element style) -> recompileDomain element style client
--         Nothing -> logError client "Error reading JSON"
--         -- TODO: might need to return to `loop`
-- recompileDomain :: String -> String -> Client -> IO ()
-- recompileDomain _ _ client@(_, conn, Renderer s) = do
--     logError client "Cannot change domains in Renderer mode."
--     loop client
-- recompileDomain element style client@(clientID, conn, Editor {}) = do
--     logInfo client "Switching to another domain..."
--     logInfo client $ "Element program received: " ++ element
--     logInfo client $ "Style program received: " ++ style
--     let elementRes = parseElement "" element
--     case elementRes of
--         Right elementEnv -> do
--             -- Send Env to the frontend for language services (for now, bag-of-word autocompletion)
--             wsSendPacket conn $ Packet { typ = "env", contents = elementEnv}
--             let styRes = parseStyle "" style elementEnv
--             case styRes of
--                 Right styProg -> do
--                     logDebug client ("Style AST:\n" ++ ppShow styProg)
--                     let client = (clientID, conn, Editor elementEnv styProg Nothing)
--                     waitSubstance client
--                 Left err -> styleError client err
--         Left err -> elementError client err
-- substanceEdit :: String -> Bool -> Client -> IO ()
-- substanceEdit subIn _ client@(_, _, Renderer _) =
--     logError client "Server Error: the Substance program cannot be updated when the server is in Renderer mode."
-- substanceEdit subIn auto client@(clientID, conn, Editor env styProg s) = do
--     logInfo client $ "Substance program received: " ++ subIn
--     let subRes = Substance.parseSubstance "" (Sugarer.sugarStmts subIn env) env
--     case subRes of
--         Right subOut@(Substance.SubOut _ (subEnv, _) _) -> do
--             logInfo client $ show subOut
--             -- TODO: store the Style values to reuse on Substance edit
--             -- TODO: pass in any new optimization config values here?
--             wsSendPacket conn $ Packet { typ = "env", contents = subEnv }
--             let styVals = []
--             let optConfig = case s of
--                             Nothing -> G.defaultOptConfig
--                             Just currState -> oConfig currState
--             styRes <- try (compileStyle styProg subOut styVals optConfig)
--             case styRes of
--                 Right newState -> do
--                     wsSendFrame conn Frame {
--                         flag = "initial",
--                         ordering = shapeOrdering newState,
--                         shapes = shapesr newState :: [Shape Double]
--                     }
--                     waitUpdate (clientID, conn, Editor env styProg $ Just newState { G.autostep = auto })
--                 Left styError -> styleRuntimeError client styError
--         Left subError -> substanceError client subError
-- updateShapes :: [Shape Double] -> Client -> IO ()
-- updateShapes newShapes client@(clientID, conn, clientState) =
--     let polyShapes = toPolymorphics newShapes
--         uninitVals = map G.toTagExpr $ G.shapes2vals polyShapes $ G.uninitializedPaths s
--         trans' = G.insertPaths (G.uninitializedPaths s) uninitVals (G.transr s)
--         -- -- Respect the optimization policy
--         -- TODO: rewrite this such that it works with the new overallObjFn
--         -- policyFns = currFns $ policyParams s
--         -- newObjFn = G.genObjfn (castTranslation trans') (filter isObjFn policyFns) (filter isConstr policyFns) (G.varyingPaths s)
--         varyMapNew = G.mkVaryMap (G.varyingPaths s) (G.varyingState s)
--         news = s {
--             G.shapesr = polyShapes,
--             G.varyingState = G.shapes2floats polyShapes varyMapNew $ G.varyingPaths s,
--             G.transr = trans',
--             -- G.paramsr = (G.paramsr s) { G.weight = G.initWeight, G.optStatus = G.NewIter, G.overallObjFn = newObjFn, G.bfgsInfo = G.defaultBfgsParams }}
--             G.paramsr = (G.paramsr s) { G.weight = G.initWeight, G.optStatus = G.NewIter, G.bfgsInfo = G.defaultBfgsParams }}
--         nextClientS = updateState clientState news
--         client' = (clientID, conn, nextClientS)
--     in if autostep s
--         then stepAndSend client'
--         else loop client'
--     where s = getBackendState clientState
-- dragUpdate :: String -> Float -> Float -> Client -> IO ()
-- dragUpdate name xm ym client@(clientID, conn, clientState) =
--     let (xm', ym') = (r2f xm, r2f ym)
--         newShapes  = map (\shape ->
--             if getName shape == name
--                 then dragShape shape xm' ym'
--                 else shape)
--             (G.shapesr s)
--         varyMapNew = G.mkVaryMap (G.varyingPaths s) (G.varyingState s)
--         news = s { G.shapesr = newShapes,
--                    G.varyingState = G.shapes2floats newShapes varyMapNew $ G.varyingPaths s,
--                    G.paramsr = (G.paramsr s) { G.weight = G.initWeight, G.optStatus = G.NewIter, G.bfgsInfo = G.defaultBfgsParams }}
--         nextClientS = updateState clientState news
--         client' = (clientID, conn, nextClientS)
--     in if autostep s
--         then stepAndSend client'
--         else loop client'
--     where s = getBackendState clientState
-- dragShape :: Autofloat a => Shape a -> a -> a -> Shape a
-- dragShape shape dx dy
--     | shape `is` "Line" =
--         trRaw "here"  $ move "startX" dx $
--         move "startY" dy $
--         move "endX"   dx $
--         move "endY"   dy shape
--     | shape `is` "Image" =
--         move "centerX" dx $
--         move "centerY" dy shape
--     | shape `is` "Arrow" =
--         move "startX" dx $
--         move "startY" dy $
--         move "endX"   dx $
--         move "endY"   dy shape
--     | shape `is` "Curve" =
--       movePath (dx, dy) shape
--     | otherwise = setX (FloatV (dx + getX shape)) $ setY (FloatV (dy + getY shape)) shape
-- move :: Autofloat a => PropID -> a -> Shape a -> Shape a
-- move prop dd s = set s prop (FloatV (dd + getNum s prop))
-- executeCommand :: String -> Client -> IO ()
-- executeCommand cmd client@(clientID, conn, clientState)
--     | cmd == "resample" = resampleAndSend client
--     | cmd == "step"     = stepAndSend client
--     | cmd == "autostep" =
--         let os  = getBackendState clientState
--             os' = os { autostep = not $ autostep os }
--         in loop (clientID, conn, updateState clientState os')
--     | otherwise         = logError client ("Can't recognize command " ++ cmd)
-- resampleAndSend, stepAndSend :: Client -> IO ()
-- resampleAndSend client@(clientID, conn, clientState) = do
--     -- Sample several states and choose the one with lowest energy
--     let news = G.resampleBest G.numStateSamples s
--     let (newShapes, _, _) = evalTranslation news
--     wsSendFrame conn
--         Frame {
--             flag = "initial",
--             ordering = shapeOrdering news,
--             shapes = newShapes
--         }
--     let nextClientS = updateState clientState news
--     let client' = (clientID, conn, nextClientS)
--     -- NOTE: could have called `loop` here, but this would result in a race condition between autostep and updateShapes somehow. Therefore, we explicitly transition to waiting for an update on image/label sizes whenever resampled.
--     waitUpdate client'
--     where s = getBackendState clientState
-- stepAndSend client@(clientID, conn, clientState) = do
-- --------------------------------------------------------------------------------
-- -- DEBUG: performance test for JSON encode/decode speed
--     -- let s' = getBackendState clientState
--     -- let s = unsafePerformIO $ do
--     --         B.writeFile "state.json" (encode s')
--     --         stateStr <- B.readFile "state.json"
--     --         return (fromMaybe (error "json decode error") $ decode stateStr)
--     -- let nexts = O.step s
-- --------------------------------------------------------------------------------
--     -- COMBAK: revert
--     let s = getBackendState clientState
--     let nexts = O.step s
--     wsSendFrame conn
--         Frame {
--             flag = "running",
--             ordering = shapeOrdering s,
--             shapes = shapesr s :: [Shape Double]
--         }
--     -- loop conn (trRaw "state:" nexts)
--     loop (clientID, conn, updateState clientState nexts)
