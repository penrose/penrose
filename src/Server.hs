{-# LANGUAGE OverloadedStrings #-}

module Server where
import Data.Aeson
import GHC.Generics
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Runtime as R
import qualified Network.WebSockets as WS
import GHC.Float (float2Double)
import Control.Exception
import System.Time
-- import System.Posix.Unistd(usleep)


type ServerState = R.State

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
    loop conn (step s)

loop :: WS.Connection -> R.State -> IO ()
loop conn s = do
            -- msg <- WS.receiveData conn
            -- case msg of
            --     ("a" :: Text) -> stepAndSend conn s
            --     ("s" :: Text) -> stepAndSend conn s
            --     ("r" :: Text) -> resampleAndSend conn s
            if R.epDone s then do
                print "Done."
                wsSendJSON conn (R.objs s)
            else if R.autostep s then
                stepAndSend conn s
            else do
                stepAndSend conn s
                -- wsSendJSON conn (R.objs s)
                -- loop conn s

resampleAndSend, stepAndSend :: WS.Connection -> R.State -> IO ()
resampleAndSend conn s = return ()
stepAndSend conn s = do
    let nexts = step s
    wsSendJSON conn (R.objs nexts)
    loop conn nexts

step :: R.State -> R.State
step s = s { R.objs = objs', R.params = params' }
        where (objs', params') = R.stepObjs (float2Double R.calcTimestep) (R.params s) (R.objs s)
