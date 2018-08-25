-- | "Server" contains functions that serves the Penrose runtime over
--   websockets connection.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

module Server where
import Utils (Autofloat, divLine, r2f, trRaw)
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
import qualified Shapes  as SD
import qualified Style   as NS
import qualified Optimizer as O
import qualified Data.Map  as M
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Network.WebSockets        as WS
import qualified Network.Socket            as S
import qualified Network.WebSockets.Stream as Stream
import qualified Control.Exception         as Exc (catch, ErrorCall)


-- COMBAK: remove TagExpr

-- Types used by the server, mainly for translation to JSON
type ServerState = NS.RState

data Feedback = Cmd Command
                | Drag DragEvent
                | Update UpdateShapes
     deriving (Generic)

data Command = Command { command :: String }
     deriving (Show, Generic)

data DragEvent = DragEvent { name :: String,
                             xm :: Float,
                             ym :: Float }
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
             -> NS.RState  -- initial state of Penrose Runtime
             -> IO ()
servePenrose domain port initState = do
     putStrLn "Starting Server..."
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

application :: ServerState -> WS.ServerApp
application s pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30 -- To keep the connection alive
    wsSendJSONList conn (NS.shapesr s)
    loop conn (O.step s)

loop :: WS.Connection -> NS.RState -> IO ()
loop conn s
    | NS.optStatus (NS.paramsr s) == NS.EPConverged = do
        putStrLn "Optimization completed."
        putStrLn ("Current weight: " ++ (show $ NS.weight (NS.paramsr s)))
        wsSendJSONFrame conn (Frame { flag = "final",
                                shapes = (NS.shapesr s) :: ([SD.Shape Double]) })
        processCommand conn s
    | NS.autostep s = stepAndSend conn s
    | otherwise = processCommand conn s

processCommand :: WS.Connection -> NS.RState -> IO ()
processCommand conn s = do
    putStrLn "Receiving Commands"
    msg_json <- WS.receiveData conn
    print msg_json
    divLine
    case decode msg_json of
        Just e -> case e of
            Cmd (Command cmd)  -> executeCommand cmd conn s
            Drag (DragEvent name xm ym)  -> dragUpdate name xm ym conn s
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
    SD.PathV xs  -> SD.PathV $ map (\(x,y) -> (r2f x, r2f y)) xs
    SD.ColorV x  -> SD.ColorV x
    SD.FileV x   -> SD.FileV x
    SD.StyleV x  -> SD.StyleV x

updateShapes :: [SD.Shape Double] -> Connection -> NS.RState -> IO ()
updateShapes newShapes conn s =
    let polyShapes = toPolymorphics newShapes
        uninitVals = map NS.toTagExpr $ NS.shapes2vals polyShapes $ NS.uninitializedPaths s
        trans' = NS.insertPaths (NS.uninitializedPaths s) uninitVals (NS.transr s)
        newObjFn = NS.genObjfn trans' (NS.objFns s) (NS.constrFns s) (NS.varyingPaths s)
        news = s {
            NS.shapesr = polyShapes,
            NS.varyingState = NS.shapes2floats polyShapes $ NS.varyingPaths s,
            NS.transr = trans',
            NS.paramsr = (NS.paramsr s) { NS.weight = NS.initWeight, NS.optStatus = NS.NewIter, NS.overallObjFn = newObjFn }}
    in if NS.autostep s then stepAndSend conn news else loop conn news

dragUpdate :: String -> Float -> Float -> WS.Connection -> NS.RState -> IO ()
dragUpdate name xm ym conn s =
    let (xm', ym') = (r2f xm, r2f ym)
        newShapes  = map (\shape ->
            if SD.getName shape == name
                then SD.setX (SD.FloatV (xm' + SD.getX shape)) $ SD.setY (SD.FloatV (ym' + SD.getY shape)) shape
                else shape)
            (NS.shapesr s)
        news = s { NS.shapesr = newShapes,
                   NS.varyingState = NS.shapes2floats newShapes $ NS.varyingPaths s,
                   NS.paramsr = (NS.paramsr s) { NS.weight = NS.initWeight, NS.optStatus = NS.NewIter }}
   in if NS.autostep s then stepAndSend conn news else loop conn news

executeCommand :: String -> WS.Connection -> NS.RState -> IO ()
executeCommand cmd conn s
    | cmd == "resample" = resampleAndSend conn s
    | cmd == "step"     = stepAndSend conn s
    | cmd == "autostep" = loop conn (s { NS.autostep = not $ NS.autostep s })
    | otherwise         = putStrLn ("Can't recognize command " ++ cmd)

resampleAndSend, stepAndSend :: WS.Connection -> NS.RState -> IO ()
resampleAndSend conn s = do
    let (newShapes, rng') = SD.sampleShapes (NS.rng s) (NS.shapesr s)
    let uninitVals = map NS.toTagExpr $ NS.shapes2vals newShapes $ NS.uninitializedPaths s
    let trans' = NS.insertPaths (NS.uninitializedPaths s) uninitVals (NS.transr s)
                    -- TODO: shapes', rng' = NS.sampleConstrainedState (NS.rng s) (NS.shapesr s) (NS.constrs s)
    let nexts = s { NS.shapesr = newShapes, NS.rng = rng',
                    NS.transr = trans',
                    NS.varyingState = NS.shapes2floats newShapes $ NS.varyingPaths s,
                    NS.paramsr = (NS.paramsr s) { NS.weight = NS.initWeight, NS.optStatus = NS.NewIter } }
    wsSendJSONList conn $ fst $ NS.evalTranslation nexts
    loop conn nexts

stepAndSend conn s = do
    let nexts = O.step s
    wsSendJSONList conn (NS.shapesr nexts :: [SD.Shape Double])
    -- loop conn (trRaw "state:" nexts)
    loop conn nexts
