{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where
import Data.Aeson
import GHC.Generics
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
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

data Message = Message { command :: String } deriving (Show, Generic)
instance FromJSON Message


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
    | R.epDone s = do
        putStrLn "Optimization completed."
        wsSendJSON conn (R.objs s)
        processCommand conn s
    | R.autostep s = stepAndSend conn s
    | otherwise = processCommand conn s

processCommand :: WS.Connection -> R.State -> IO ()
processCommand conn s = do
    putStrLn "Receiving Commands"
    msg_json <- WS.receiveData conn
    putStrLn "Command received"
    let msg = fromMaybe Message { command = "Error reading JSON" } (decode msg_json :: Maybe Message)
    let cmd = command msg
    case cmd of
        _   | cmd == "resample" -> resampleAndSend conn s
            | cmd == "step"     -> stepAndSend conn s
            | cmd == "autostep" -> do
                putStrLn "AUTOSTEP"
                loop conn (s { R.autostep = not $ R.autostep s })
                -- | otherwise -> putStrLn ("Can't recognize command " ++ cmd)

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
