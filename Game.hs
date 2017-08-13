--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE GADTs #-}
module Game(Player(..),other,Game(..),MetaData(..),Status(..),PlayerID,GameID) where

import Data.Aeson(ToJSON(..),Value,object)
import Data.Text(pack)
import Data.Aeson.Types(Pair)
import Data.Bits((.&.))
--import Control.Monad.State
import Network.WebSockets(Connection)
--import Data.ByteString(ByteString)

class Game g where
    --makeMove assumes the move is made by the player whose turn it is
    makeMove :: String -> g -> Status -> Either String (g,Status)
    getData :: g -> Value
    newGame :: g


type PlayerID = String
type GameID = String


data MetaData = MD {
    player0 :: PlayerID,
    player1 :: PlayerID,
    listeners :: [Connection],
    gid :: GameID,
    status :: Status}--OK, turn technically isn't metadata



(.:) :: ToJSON b => String->b->Pair
a .: b = (pack a,toJSON b)

updateMsg :: Game g => g -> MetaData -> Value
updateMsg gg dat  = object
    ["request" .: "update",
     "data" .: (getData  gg),
     "gameID" .: gid dat,
     "state" .: fromEnum (status dat)]

data Player = Zero | One deriving (Eq,Show,Enum)

other :: Player -> Player
other One = Zero
other Zero = One


data Status = Unstarted | IsTurn Player | Won Player | Draw | Timeout Player
  deriving (Eq,Show)

gameover :: Status -> Bool
gameover s = fromEnum s .&. gAMEOVER /= 0
{-gameover (Won _)     = True
gameover Draw        = True
gameover (Timeout _) = True
gameover Unstarted   = False
gameover (IsTurn _)  = False-}


instance Enum Status where
    toEnum _ = error "unused method"

    fromEnum (Won p) = sTARTED+gAMEOVER+tURN*(fromEnum p)
    fromEnum (IsTurn p) = sTARTED+tURN*(fromEnum p)
    fromEnum (Timeout p) = tIMEUP + gAMEOVER + tURN*(fromEnum p)
    fromEnum Unstarted = 0
    fromEnum Draw = gAMEOVER + dRAW

sTARTED=4
aIP=2
gAMEOVER=8
dRAW=16
tURN=1
tIMEUP=32

--instance Game g => Game (StoredGame g) where
