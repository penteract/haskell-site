--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE GADTs #-}
module Game(Player,Game(..),StoredGame(..),Status(..)) where

import Data.Aeson(ToJSON(..),Value,object)
import Data.Text(pack)
import Data.Aeson.Types(Pair)
--import Control.Monad.State
import Network.WebSockets(Connection)
--import Data.ByteString(ByteString)

class Game g where
    --makeMove assumes the move is made by the player whose turn it is
    makeMove :: Value -> g -> Status -> Either String (g,Status)
    getData :: g -> Value
    newGame :: g


type PlayerID = String
type GameID = String


data StoredGame g = SG {
    game :: g,
    player0 :: PlayerID,
    player1 :: PlayerID,
    listeners :: [Connection],
    gid :: GameID,
    status :: Status} -- -> StoredGame g

(.:) :: ToJSON b => String->b->Pair
a .: b = (pack a,toJSON b)

updateMsg :: Game g => StoredGame g -> Value
updateMsg gg = object
    ["request" .: "update",
     --"data" .: getData $ game  gg,
     "gameID" .: gid gg,
     "state" .: fromEnum (status gg)]

data Player = Zero | One deriving (Eq,Show)

other :: Player -> Player
other One = Zero
other Zero = One

instance Enum Player where

    toEnum 1 = One
    toEnum 0 = Zero
    toEnum _ = error "player must be 0 or 1"

    fromEnum One = 1
    fromEnum Zero = 0


data Status = Unstarted | IsTurn Player | Won Player | Draw | Timeout Player deriving (Eq,Show)

gameover :: Status -> Bool
gameover (Won _)     = True
gameover Draw        = True
gameover (Timeout _) = True
gameover Unstarted   = False
gameover (IsTurn _)  = False


instance Enum Status where
    toEnum _ = error "unused method"

    fromEnum (Won p) = sTARTED+gAMEOVER+tURN*(fromEnum p)
    fromEnum (IsTurn p) = sTARTED+tURN*(fromEnum p)
    fromEnum (Timeout p) = tIMEUP + tURN*(fromEnum p)
    fromEnum (Unstarted) = 0
    fromEnum (Draw) = gAMEOVER + dRAW


sTARTED=4
aIP=2
gAMEOVER=8
dRAW=16
tURN=1
tIMEUP=32





--instance Game g => Game (StoredGame g) where
