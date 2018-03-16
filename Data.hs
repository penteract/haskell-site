module Data(PlayerID, GameID, MetaData(..), newMD,
    GameStore(..), GameStoreList(..), Game.Game(..),
    Status(..),Player(..),
    GameInfo(..), games, ox)
    where

import OX
import qualified Game
import Game hiding (makeMove,getData,newGame)

import Data.Aeson(Value)
import Control.Concurrent.MVar
import qualified Control.Concurrent.Map as Map


--Generalised game, a wrapper around different game types
--data GGame = OXG OX MetaData  -- | EnsquaredG MetaData Ensquared

type GameStore g = Map.Map GameID (MVar (g,MetaData))

data GameStoreList = GSL {
    getOXStore ::GameStore OX
}

data GameInfo = GameInfo{
    tag :: String,
    name :: String
    --makeMove :: String -> GGame -> Either String GGame,
    --getData :: GGame -> Value,
    --newGame :: PlayerID -> PlayerID -> GameID -> GGame
}


{-IDEA: create a typeclass KnownGame that

typeclass Game g => KnownGame g where
    getStore :: GameStoreList -> GameStore g


-}

--(...) ::

--newGame :: String -> Maybe GGame
--newGame "ox3" n = OXG ... MD newGame

ox = GameInfo{
    tag     = "ox3",
    name    = "3D Noughts and Crosses"{-,
    makeMove = \pos (OXG game md) -> --unsafe pattern match
        (\(g,s) -> OXG g (md{status=s}) ) <$> Game.makeMove pos game (status md),
    getData  = (\ (OXG game _) -> Game.getData game),
    newGame  = \pl0 pl1 gid -> OXG Game.newGame MD{
        player0   = pl0,
        player1   = pl1,
        listeners = [],
        gid       = gid,
        status    = Unstarted-}
}

games = [ox]
