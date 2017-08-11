module Data(games,GameStore) where

import OX
import Game

import Control.Concurrent.MVar
import qualified Control.Concurrent.Map as Map


--Generalised game, a wrapper around different game types
data GGame = OXG (StoredGame OX)
    
type GameStop
type GameStore = Map.Map GameID (MVar GGame)


(...) :: 

newGame :: String -> Maybe GGame
newGame "ox3" n = OXG ... SG newGame