-- | "Server" contains functions that serves the Penrose runtime over
--   websockets connection.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

module Server where
import Shapes
import Computation
import Utils (Autofloat, r2f)
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
import qualified NewStyle as NS
import qualified Optimizer as O
import qualified Data.Map as M
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Network.WebSockets        as WS
import qualified Network.Socket            as S
import qualified Network.WebSockets.Stream as Stream
import qualified Control.Exception         as Exc (catch, ErrorCall)


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
                       shapes :: [NS.Shape Double] 
                    } deriving (Show, Generic)

data Frame = Frame { flag :: String, 
                     shapes :: [NS.Shape Double] 
                   } deriving (Show, Generic)

-- TODO write these. do we need FromJSON instance for "a"? or should we just convert to floats?
instance FromJSON (NS.TagExpr a) where
         parseJSON = error "TODO fromjson tagexpr"

instance ToJSON (NS.TagExpr a) where
         toJSON x = error "TODO tojson tagexpr"

instance FromJSON Feedback
instance FromJSON Command
instance FromJSON DragEvent
instance FromJSON UpdateShapes
instance ToJSON Frame

wsSendJSON :: WS.Connection -> (NS.Shape Double) -> IO ()
wsSendJSON conn shape = WS.sendTextData conn $ encode shape

-- TODO use the more generic wsSendJSON?
wsSendJSONList :: WS.Connection -> ([NS.Shape Double]) -> IO ()
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
                                shapes = (NS.shapesr s) :: ([NS.Shape Double]) })
        processCommand conn s
    | NS.autostep s = stepAndSend conn s
    | otherwise = processCommand conn s

processCommand :: WS.Connection -> NS.RState -> IO ()
processCommand conn s = do
    --putStrLn "Receiving Commands"
    msg_json <- WS.receiveData conn
    --print msg_json
    case decode msg_json of
        Just e -> case e of
            Cmd (Command cmd)  -> executeCommand cmd conn s
            Drag (DragEvent name xm ym)  -> error "TODO: IMPLEMENT DRAG UPDATE" -- dragUpdate name xm ym conn s
            Update (UpdateShapes shapes)  -> updateShapes shapes conn s
        Nothing -> error "Error reading JSON"

toPolymorphics :: [NS.Shape Double] -> (forall a . (Autofloat a) => [NS.Shape a])
toPolymorphics = map toPolymorphic
   where toPolymorphic :: NS.Shape Double -> (forall a . (Autofloat a) => NS.Shape a)
         toPolymorphic (ctor, properties) = (ctor, M.map toPolyProperty properties)

         toPolyProperty :: NS.TagExpr Double -> (forall a . (Autofloat a) => NS.TagExpr a)
         toPolyProperty e@(NS.OptEval v) = NS.OptEval v
         toPolyProperty e@(NS.Done v) =
               case v of
               TNum n -> NS.Done $ TNum $ r2f n
               -- Not sure why these have to be rewritten from scratch...
               TBool x -> NS.Done $ TBool x
               TStr x ->  NS.Done $ TStr x
               TInt x ->  NS.Done $ TInt x
               _ -> error "finish double -> polymorphic value conversion" -- TODO finish
               -- TList x ->  NS.Done $ TList x
               -- TPt x ->  NS.Done $ TPt x

updateShapes :: ([NS.Shape Double]) -> Connection -> NS.RState -> IO ()
updateShapes newShapes conn s = if NS.autostep s then stepAndSend conn news else loop conn news
    where
        news = s { NS.shapesr = toPolymorphics newShapes,
                   NS.paramsr = (NS.paramsr s) { NS.weight = NS.initWeight, NS.optStatus = NS.NewIter }}

-- TODO implement this
-- dragUpdate :: String -> Float -> Float -> WS.Connection -> NS.RState -> IO ()
-- dragUpdate name xm ym conn s = if NS.autostep s then stepAndSend conn news else loop conn news
--     where
--         newShapes = map (\x ->
--                 if getName x == name
--                     then setX (xm + getX x) $ setY (-ym + getY x) x
--                     else x)
--             (NS.shapesr s)
--         news = s { NS.shapesr = newShapes,
--                    NS.paramsr = (NS.paramsr s) { NS.weight = NS.initWeight, NS.optStatus = NS.NewIter }}

executeCommand :: String -> WS.Connection -> NS.RState -> IO ()
executeCommand cmd conn s
    | cmd == "resample" = resampleAndSend conn s
    | cmd == "step"     = stepAndSend conn s
    | cmd == "autostep" = loop conn (s { NS.autostep = not $ NS.autostep s })
    | otherwise         = putStrLn ("Can't recognize command " ++ cmd)

resampleAndSend, stepAndSend :: WS.Connection -> NS.RState -> IO ()
resampleAndSend conn s = do
    let shapes' = NS.shapesr s
                    -- TODO: shapes', rng' = NS.sampleConstrainedState (NS.rng s) (NS.shapesr s) (NS.constrs s)
    let nexts = s { NS.shapesr = shapes', -- NS.rng = rng',
                    NS.paramsr = (NS.paramsr s) { NS.weight = NS.initWeight, NS.optStatus = NS.NewIter } }
    wsSendJSONList conn shapes'
    loop conn nexts

stepAndSend conn s = do
    let nexts = O.step s
    wsSendJSONList conn ((NS.shapesr nexts) :: ([NS.Shape Double]))
    loop conn nexts
