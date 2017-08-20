{-# LANGUAGE OverloadedStrings #-}
module WSApp where

import Network.WebSockets
import Network.HTTP.Types
import Data.Text(Text)
import Data.ByteString.Lazy(ByteString)
import qualified Control.Concurrent.Map as Map

import Data(GameStoreList)

wsApp :: GameStoreList -> Query -> ServerApp
wsApp m q pending_conn = do
    putStrLn (show q)
    putStrLn (show $ requestPath $ pendingRequest pending_conn)
    conn <- acceptRequest pending_conn
    forkPingThread conn 30
    sendTextData conn ("Hello, client!" :: ByteString)
    procRequests m conn


procRequests :: GameStoreList -> Connection -> IO ()
procRequests m conn = do
    dat <- receiveData conn
    putStrLn (show (dat::ByteString))
    procRequests m conn
