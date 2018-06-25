{-# LANGUAGE OverloadedStrings #-}
module WSApp where

import Network.WebSockets
import Network.HTTP.Types
import qualified Data.Text as Text(pack)
import Data.ByteString.Char8(ByteString,pack)

import qualified Control.Concurrent.Map as Map
import Control.Concurrent
import Text.JSON(toJSObject,JSValue(JSObject),encodeStrict)
import Control.Applicative -- .Zip
import Control.Monad
import Data.Traversable

import Data --(games,GameStoreList,Retriveable(Retrive))
import Game(updateMsg,Game)

putInto :: (Traversable t,Monad t,Monad m) => t (m (t a)) -> m (t a)
putInto xs = join <$> sequence xs


wsApp :: GameStoreList -> Query -> ServerApp
wsApp store q pending_conn = do
    putStrLn "Websocket request"
    putStrLn (show q)
    let path = requestPath $ pendingRequest pending_conn
    let gpath = head.fst$ decodePath path
    putStrLn (show $ gpath)
    --filterby
    let getParam s = join$ lookup s q
    let r = lookup gpath [(Text.pack t,r) | (r,GameInfo{tag=t}) <-games]

    putStrLn "Got Here"
    --let f gid pid (Retrive ret) = ((,) pid <$> ) <$> (Map.lookup gid (ret store)) -- doesn't work because of scoping
    --maybeStoreAndPID <- putInto$ liftA3 f (getParam "gameID") (getParam "playerID") r
    --let liftA3 (,,) (getParam "gameID") (getParam "playerID") r
    case liftA3 (,,) (getParam "gameID") (getParam "playerID") r  of -- liftA2 (,) == mzip for Maybe
        Nothing -> do
            putStrLn "rejecting"
            rejectRequest pending_conn "bad request"
        Just (gid,pid,Retrive ret) -> do
            mg <- Map.lookup gid (ret store)
            putStrLn "found map"
            case mg of
                Nothing -> rejectRequest pending_conn "game not found"
                Just g -> do -- IO
                    conn <- acceptRequest pending_conn
                    forkPingThread conn 30
                    sendTextData conn ("{6:\"hi\"}" :: ByteString)
                    forkIO$ (newEmptyMVar >>= (\x ->sendUpdates g x (const True) conn))
                    procRequests g conn


procRequests ::Game g=> X (g,MetaData) -> Connection -> IO ()
procRequests m conn = do
    dat <- receiveData conn
    putStrLn (show (dat::ByteString))
    procRequests m conn

sendUpdates :: Game g=> X (g,MetaData) -> MVar (g,MetaData) -> ((g,MetaData) -> Bool) -> Connection -> IO ()
sendUpdates x w isNew conn = do
    (g,dat@MD{status=s}) <- checkUpdate x w isNew
    putStrLn$ show s
    sendTextData conn (pack$ encodeStrict$ JSObject$toJSObject$ updateMsg g dat)
    sendUpdates x w (/=(g,dat)) conn
    --sendUpdates x w (\(g',MD{})/= v') conn

getMoves :: Game g => X (g,MetaData) -> Connection -> IO ()
getMoves x conn = do
        dat <- receiveData conn
        putStrLn (show (dat::ByteString))
        getMoves x conn
