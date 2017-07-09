{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where
import Shapes
import GHC.Generics
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Runtime as R
import qualified Network.WebSockets as WS
import GHC.Float (float2Double)
import Control.Exception
import System.Time
-- import System.Posix.Unistd(usleep)
import Data.Char (isPunctuation, isSpace)
import Data.Aeson
import Data.Maybe (fromMaybe)


type ServerState = R.State
data FeedBack = Cmd Command | Drag DragEvent | Update UpdateShapes deriving (Show, Generic)
data Command = Command { command :: String } deriving (Show, Generic)
data DragEvent = DragEvent { name :: String, xm :: Float, ym :: Float } deriving (Show, Generic)
data UpdateShapes = UpdateShapes { objs :: [Obj] } deriving (Show, Generic)
instance FromJSON FeedBack
instance FromJSON Command
instance FromJSON DragEvent
instance FromJSON UpdateShapes


wsSendJSON :: ToJSON j => WS.Connection -> j -> IO ()
wsSendJSON conn obj = WS.sendTextData conn $ encode obj

-- wsReceiveJSON :: (WS.TextProtocol p, FromJSON j) => WS.WebSockets p (Maybe j)
-- wsReceiveJSON = fmap decode WS.receiveData

servePenrose :: String -> Int -> R.State -> IO ()
servePenrose domain port initState = do
    putStrLn "Starting Server..."
    WS.runServer domain port $ application initState

application :: ServerState -> WS.ServerApp
application s pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30 -- To keep the connection alive
    wsSendJSON conn (R.objs s)
    loop conn (step s)

loop :: WS.Connection -> R.State -> IO ()
loop conn s
    | R.optStatus ( R.params s) == R.EPConverged = do
        putStrLn "Optimization completed."
        putStrLn ("Current weight: " ++ (show $ R.weight (R.params  s)))
        -- wsSendJSON conn (R.objs s) -- TODO: is this necessary?
        processCommand conn s
    | R.autostep s = stepAndSend conn s
    | otherwise = processCommand conn s

processCommand :: WS.Connection -> R.State -> IO ()
processCommand conn s = do
    -- putStrLn "Receiving Commands"
    msg_json <- WS.receiveData conn
    case decode msg_json of
        Just e -> case e of
            Cmd (Command cmd)  -> executeCommand cmd conn s
            Drag (DragEvent name xm ym)  -> dragUpdate name xm ym conn s
            Update (UpdateShapes objs)  -> updateShapes objs conn s
        Nothing -> error "Error reading JSON"

updateShapes :: [Obj] -> WS.Connection -> R.State -> IO ()
updateShapes newObjs conn s = if R.autostep s then stepAndSend conn news else loop conn news
    where
        news = s { R.objs = newObjs, R.params = (R.params s) { R.weight = R.initWeight, R.optStatus = R.NewIter }}

dragUpdate :: String -> Float -> Float -> WS.Connection -> R.State -> IO ()
dragUpdate name xm ym conn s = if R.autostep s then stepAndSend conn news else loop conn news
    where
        newObjs = map (\x ->
                if getName x == name
                    then setX (xm + getX x) $ setY (-ym + getY x) x
                    else x)
            (R.objs s)
        news = s { R.objs = newObjs, R.params = (R.params s) { R.weight = R.initWeight, R.optStatus = R.NewIter }}

executeCommand :: String -> WS.Connection -> R.State -> IO ()
executeCommand cmd conn s
    | cmd == "resample" = resampleAndSend conn s
    | cmd == "step"     = stepAndSend conn s
    | cmd == "autostep" = loop conn (s { R.autostep = not $ R.autostep s })
    | otherwise         = putStrLn ("Can't recognize command " ++ cmd)


resampleAndSend, stepAndSend :: WS.Connection -> R.State -> IO ()
resampleAndSend conn s = do
    let (objs', rng') = R.sampleConstrainedState (R.rng s) (R.objs s) (R.constrs s)
    let nexts = s { R.objs = objs', R.down = False, R.rng = rng', R.params = (R.params s) { R.weight = R.initWeight, R.optStatus = R.NewIter } }
    wsSendJSON conn (R.objs nexts)
    loop conn nexts
stepAndSend conn s = do
    let nexts = step s
    wsSendJSON conn (R.objs nexts)
    loop conn nexts

step :: R.State -> R.State
step s = s { R.objs = objs', R.params = params' }
        where (objs', params') = R.stepObjs (float2Double R.calcTimestep) (R.params s) (R.objs s)
