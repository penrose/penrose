-- | "Server" contains functions that serves the Penrose runtime over
--   websockets connection.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

module Server where
import Utils (Autofloat, divLine, r2f, trRaw, fromRight)
import GHC.Generics
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception
import Control.Monad (forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, forkIOWithUnmask)
import Data.Char (isPunctuation, isSpace)
import Data.Aeson
import Data.Maybe (fromMaybe)
import GHC.Float (float2Double)
import Network.WebSockets.Connection
import System.Time
import System.Random
import Debug.Trace
import Env
import qualified Shapes  as SD
import qualified Style   as NS
import qualified Substance     as SU
import qualified GenOptProblem as G
import qualified Optimizer as O
import qualified Data.Map  as M
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Network.WebSockets        as WS
import qualified Network.Socket            as S
import qualified Network.WebSockets.Stream as Stream
import qualified Control.Exception         as Exc (catch, ErrorCall)
import           System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)

-- COMBAK: remove TagExpr

-- Types used by the server, mainly for translation to JSON
-- TODO model this differently?
data ServerState = ServerState {
    optState :: Maybe G.State,
    env :: Maybe Env.VarEnv,
    sty :: Maybe NS.StyProg
} deriving (Generic)

data Feedback
    = Cmd Command
    | Drag DragEvent
    | Update UpdateShapes
    | Edit SubstanceEdit
    deriving (Generic)

data Command = Command { command :: String }
     deriving (Show, Generic)

data DragEvent = DragEvent { nama :: String,
                             xm :: Float,
                             ym :: Float }
     deriving (Show, Generic)

data SubstanceEdit = SubstanceEdit { program :: String }
     deriving (Show, Generic)

data UpdateShapes = UpdateShapes {
                       shapes :: [SD.Shape Double]
                    } deriving (Show, Generic)

data Frame = Frame { flag :: String,
                     shapes :: [SD.Shape Double]
                   } deriving (Show, Generic)

instance FromJSON Feedback
instance FromJSON Command
instance FromJSON DragEvent
instance FromJSON UpdateShapes
instance FromJSON SubstanceEdit
instance ToJSON Frame

wsSendJSON :: WS.Connection -> (SD.Shape Double) -> IO ()
wsSendJSON conn shape = WS.sendTextData conn $ encode shape

-- TODO use the more generic wsSendJSON?
wsSendJSONList :: WS.Connection -> ([SD.Shape Double]) -> IO ()
wsSendJSONList conn shapes = WS.sendTextData conn $ encode shapes

wsSendJSONFrame :: WS.Connection -> Frame -> IO ()
wsSendJSONFrame conn frame = WS.sendTextData conn $ encode frame

-- | 'servePenrose' is the top-level function that "Main" uses to start serving
--   the Penrose Runtime.
servePenrose :: String  -- the domain of the server
             -> Int  -- port number of the server
             -> G.State  -- initial state of Penrose Runtime
             -> IO ()
servePenrose domain port initState = do
     putStrLn "Starting Server..."
     let s = ServerState { optState = Just initState, env = Nothing, sty = Nothing }
     Exc.catch (runServer domain port $ application s) handler
     where
        handler :: Exc.ErrorCall -> IO ()
        handler _ = putStrLn "Server Error"

-- | 'serveWithoutSub' TODO
serveWithoutSub :: String   -- the domain of the server
                -> Int      -- port number of the server
                -> VarEnv   -- ^ TODO
                -> NS.StyProg  -- ^ TODO
                -> IO ()
serveWithoutSub domain port e styProg = do
     putStrLn "Starting Server..."
     let initState = ServerState { optState = Nothing, env = Just e, sty = Just styProg }
     Exc.catch (runServer domain port $ application initState) handler
     where
        handler :: Exc.ErrorCall -> IO ()
        handler _ = putStrLn "Server Error"

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

getOptState :: ServerState -> G.State
getOptState serverState = fromMaybe
                (error "Server error: optimization state not initialized")
                (optState serverState)


application :: ServerState -> WS.ServerApp
application serverState pending = do
    conn <- WS.acceptRequest pending
    case optState serverState of
        Nothing -> processCommand conn serverState -- Wait for first Substance command (COMBAK: make an explicit function for this)
        Just s -> do
            WS.forkPingThread conn 30 -- To keep the connection alive
            wsSendJSONList conn (G.shapesr s)
            loop conn $ serverState { optState = Just $ O.step s }

loop :: WS.Connection -> ServerState -> IO ()
loop conn serverState
    | G.optStatus (G.paramsr s) == G.EPConverged = do
        putStrLn "Optimization completed."
        putStrLn ("Current weight: " ++ (show $ G.weight (G.paramsr s)))
        wsSendJSONFrame conn (Frame { flag = "final",
                                shapes = (G.shapesr s) :: ([SD.Shape Double]) })
        processCommand conn serverState
    | G.autostep s = stepAndSend conn serverState
    | otherwise = processCommand conn serverState
    where s = getOptState serverState

processCommand :: WS.Connection -> ServerState -> IO ()
processCommand conn s = do
    putStrLn "Receiving Commands"
    msg_json <- WS.receiveData conn
    print msg_json
    divLine
    case decode msg_json of
        Just e -> case e of
            Cmd (Command cmd)             -> executeCommand cmd conn s
            Drag (DragEvent name xm ym)   -> dragUpdate name xm ym conn s
            Edit (SubstanceEdit subProg)  -> substanceEdit subProg conn s
            Update (UpdateShapes shapes)  -> updateShapes shapes conn s
        Nothing -> error "Error reading JSON"

toPolymorphics :: [SD.Shape Double] -> (forall a . (Autofloat a) => [SD.Shape a])
toPolymorphics = map toPolymorphic

toPolymorphic :: SD.Shape Double -> (forall a . (Autofloat a) => SD.Shape a)
toPolymorphic (ctor, properties) = (ctor, M.map toPolyProperty properties)

toPolyProperty :: SD.Value Double -> (forall a . (Autofloat a) => SD.Value a)
toPolyProperty v = case v of
    -- Not sure why these have to be rewritten from scratch...
    SD.FloatV n  -> SD.FloatV $ r2f n
    SD.BoolV x   -> SD.BoolV x
    SD.StrV x    -> SD.StrV x
    SD.IntV x    -> SD.IntV x
    SD.PtV (x,y) -> SD.PtV (r2f x, r2f y)
    -- TODO: rewrite this
    -- SD.PathV xs  -> SD.PathV $ map (\(x,y) -> (r2f x, r2f y)) xs
    SD.ColorV x  -> SD.ColorV x
    SD.FileV x   -> SD.FileV x
    SD.StyleV x  -> SD.StyleV x

substanceEdit :: String -> Connection -> ServerState -> IO ()
substanceEdit subIn conn s = do
    putStrLn $ bgColor Green "Substance program received: " ++ subIn
    (subProg, (subEnv, eqEnv), labelMap) <- SU.parseSubstance "" subIn $ getEnv s
    let styProg = getSty s
    let selEnvs = NS.checkSels subEnv styProg
    let subss = NS.find_substs_prog subEnv eqEnv subProg styProg selEnvs
    let trans = NS.translateStyProg subEnv eqEnv subProg styProg labelMap :: forall a . (Autofloat a) => Either [NS.Error] (NS.Translation a)
    let newState = G.genOptProblemAndState (fromRight trans)
    let warns = NS.warnings $ fromRight trans -- TODO: report warnings
    stepAndSend conn $ s { optState = Just newState }
    where
        getSty s = fromMaybe (error "Server Error: Style AST not stored") $ sty s
        getEnv s = fromMaybe (error "Server Error: Element AST not stored") $ env s

updateShapes :: [SD.Shape Double] -> Connection -> ServerState -> IO ()
updateShapes newShapes conn serverState =
    let polyShapes = toPolymorphics newShapes
        uninitVals = map G.toTagExpr $ G.shapes2vals polyShapes $ G.uninitializedPaths s
        trans' = G.insertPaths (G.uninitializedPaths s) uninitVals (G.transr s)
        newObjFn = G.genObjfn trans' (G.objFns s) (G.constrFns s) (G.varyingPaths s)
        news = s {
            G.shapesr = polyShapes,
            G.varyingState = G.shapes2floats polyShapes $ G.varyingPaths s,
            G.transr = trans',
            G.paramsr = (G.paramsr s) { G.weight = G.initWeight, G.optStatus = G.NewIter, G.overallObjFn = newObjFn }}
        nextServerS = serverState { optState = Just news }
    in if G.autostep s then stepAndSend conn nextServerS else loop conn nextServerS
    where s = getOptState serverState

dragUpdate :: String -> Float -> Float -> WS.Connection -> ServerState -> IO ()
dragUpdate name xm ym conn serverState =
    let (xm', ym') = (r2f xm, r2f ym)
        newShapes  = map (\shape ->
            if SD.getName shape == name
                then SD.setX (SD.FloatV (xm' + SD.getX shape)) $ SD.setY (SD.FloatV (ym' + SD.getY shape)) shape
                else shape)
            (G.shapesr s)
        news = s { G.shapesr = newShapes,
                   G.varyingState = G.shapes2floats newShapes $ G.varyingPaths s,
                   G.paramsr = (G.paramsr s) { G.weight = G.initWeight, G.optStatus = G.NewIter }}
        nextServerS = serverState { optState = Just news }
    in if G.autostep s then stepAndSend conn nextServerS else loop conn nextServerS
    where s = getOptState serverState

executeCommand :: String -> WS.Connection -> ServerState -> IO ()
executeCommand cmd conn s
    | cmd == "resample" = resampleAndSend conn s
    | cmd == "step"     = stepAndSend conn s
    | cmd == "autostep" =
        let os  = getOptState s
            os' = os { G.autostep = not $ G.autostep os }
        in loop conn $ s { optState = Just os' }
    | otherwise         = putStrLn ("Can't recognize command " ++ cmd)

resampleAndSend, stepAndSend :: WS.Connection -> ServerState -> IO ()
resampleAndSend conn serverState = do
    let (newShapes, rng') = SD.sampleShapes (G.rng s) (G.shapesr s)
    let uninitVals = map G.toTagExpr $ G.shapes2vals newShapes $ G.uninitializedPaths s
    let trans' = G.insertPaths (G.uninitializedPaths s) uninitVals (G.transr s)
                    -- TODO: shapes', rng' = G.sampleConstrainedState (G.rng s) (G.shapesr s) (G.constrs s)
    let nexts = s { G.shapesr = newShapes, G.rng = rng',
                    G.transr = trans',
                    G.varyingState = G.shapes2floats newShapes $ G.varyingPaths s,
                    G.paramsr = (G.paramsr s) { G.weight = G.initWeight, G.optStatus = G.NewIter } }
    wsSendJSONList conn $ fst $ G.evalTranslation nexts
    let nextServerS = serverState { optState = Just nexts }
    loop conn nextServerS
    where s = getOptState serverState

stepAndSend conn serverState = do
    let s = getOptState serverState
    let nexts = O.step s
    wsSendJSONList conn (G.shapesr nexts :: [SD.Shape Double])
    -- loop conn (trRaw "state:" nexts)
    loop conn $ serverState { optState = Just nexts }
