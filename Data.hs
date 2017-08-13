module Data(games,GameStore) where

import OX
import Game

import Control.Concurrent.MVar
import qualified Control.Concurrent.Map as Map


--Generalised game, a wrapper around different game types
data GGame = OXG OX MetaData  -- | EnsquaredG MetaData Ensquared

type GameStore = Map.Map GameID (MVar GGame)


--(...) ::

--newGame :: String -> Maybe GGame
--newGame "ox3" n = OXG ... MD newGame



games = [("ox3","3D noughts and crosses", OXG newGame )]
