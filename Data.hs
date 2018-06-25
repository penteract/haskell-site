{-# LANGUAGE OverloadedStrings,ExistentialQuantification #-}
{-Stores data for individual games. This should be the only file which must be modified to add a new game-}
{-Should consider renaming (or at least adding qualifiers) -}
module Data(PlayerID, GameID, MetaData(..), newMD,
    GameStore(..), GameStoreList(..), Game.Game(..),Retriveable(..),
    Status(..),Player(..),
    GameInfo(..), games,
    X, newXVar, readX, maybeModify, checkUpdate)
    where

import OX
import qualified Game
import Game hiding (makeMove,getData,newGame)

--import Data.Aeson(Value)
import Text.JSON
import Control.Concurrent.MVar
import Control.Exception
import qualified Control.Concurrent.Map as Map
import qualified Data.ByteString.Char8 as C
import Data.Functor(($>))



-- a data structure which stores a value and allows requests for updates
type X a = MVar (a,[MVar a])

newXVar :: a -> IO (X a)
newXVar x = newMVar (x,[])

readX :: X a -> IO a
readX x = fst <$> readMVar x

--read an XVar, possibly modifying it and returning a result
maybeModify :: X a -> (a -> (b,Maybe a)) -> IO b
maybeModify x f = do
    (v,ws) <- takeMVar x
    let (r, v') = f v
    case v' of
        Nothing -> putMVar x (v,ws) >> return r
        (Just v) -> putMVar x (v,[]) >> mapM (`putMVar` v) ws >> return r


-- | Checks if the contents is new, otherwise waits on w for an update
-- does not guarantee that 'isNew result' will hold unless modifications follow a strict order
-- Beware of the ABA problem
-- I strongly suspect that the exception handling is unnecessary in this particular application
-- it may be needed if I want to kill it when the socket closes
checkUpdate :: X a -> MVar a -> (a -> Bool) -> IO a
checkUpdate x w isNew = mask (\unmask -> do
    (v,ws) <- readMVar x
    b <- unmask$ evaluate (isNew v)-- hopefully I'm doing this right
    if b then return v
    else do
         (v,ws) <- takeMVar x
         b <- unmask (evaluate (isNew v)) `onException` putMVar x (v,ws)
         if b then putMVar x (v,ws) $>  v
              else  putMVar x (v,w:ws) >> takeMVar w
    )



type GameStore g = Map.Map GameID (X (g,MetaData))

data GameStoreList = GSL {
    getOXStore ::GameStore OX
}

data Retriveable = forall g. Game g => Retrive (GameStoreList -> GameStore g)


data GameInfo = GameInfo{
    tag   :: String,
    name  :: String,
    views :: [(C.ByteString,C.ByteString)]
}

ox = GameInfo{
    tag     = "ox3",
    name    = "3D Noughts and Crosses",
    views   = map (\x -> (x,x `C.append` ".html")) ["table","perspective"]
}

games :: [(Retriveable,GameInfo)]
games = [(Retrive getOXStore, ox)]
