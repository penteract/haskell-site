module Game(Player(..),other,Game(..),MetaData(..),Status(..),newMD,
    PlayerID,GameID,gameURL) where

import Data.Aeson(ToJSON(..),Value,object)
import Data.Text(pack)
import Data.Aeson.Types(Pair)
import Data.Bits((.&.))
--import Control.Monad.State
import Network.WebSockets(Connection)
import Data.ByteString(ByteString,unpack)
import Data.Int
import System.Posix.Time
import Utils

class Game g where
    --makeMove assumes the move is made by the player whose turn it is
    makeMove :: String -> g -> Status -> Either String (g,Status)
    getData :: g -> Value
    newGame :: g
    ais :: String -> Maybe (g -> Either String g)


type PlayerID = ByteString
type GameID = ByteString


data MetaData = MD {
    player0 :: PlayerID,
    player1 :: PlayerID,
    listeners :: [Connection],
    gid :: GameID,
    lastMove :: Int,
    status :: Status}--OK, turn isn't really metadata

newMD :: PlayerID -> PlayerID -> Player -> GameID -> IO MetaData
newMD pl0 pl1 p gid = do
    t <- fromEnum <$> epochTime --systemSeconds<$>getSystemTime
    return MD{
        player0   = pl0,
        player1   = pl1,
        listeners = [],
        gid       = gid,
        lastMove  = t,
        status    = Unstarted p}

instance ToJSON ByteString where
    toJSON b = toJSON $ unpack b

--(.:) :: ToJSON b => String->b->Pair
--a .: b = (pack a,toJSON b)


--(.::) :: ToJSON b => String->b->Pair
--a .:: b = (pack a,toJSON $ unpack b)

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

-- Waiting for p to join; Is the turn of p; p has won; result is a draw; p lost due to timeout
data Status = Unstarted Player | IsTurn Player | Won Player | Draw | Timeout Player
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
    fromEnum (Timeout p) = sTARTED + tIMEUP + gAMEOVER + tURN*(fromEnum p)
    fromEnum (Unstarted p) = tURN*(fromEnum p)
    fromEnum Draw = sTARTED + gAMEOVER + dRAW

sTARTED=4
aIP=2
gAMEOVER=8
dRAW=16
tURN=1
tIMEUP=32

gameURL :: MetaData -> String -> Maybe Player -> String--should this be in Data?
gameURL (MD{gid=n,player0=pl0,player1=pl1}) gname pl = "/{}/play?gameID={}"%gname%show n ++ case pl of
    Just p -> "&playerID={}"%show ([pl0,pl1]!!fromEnum p)
    Nothing -> ""

--instance Game g => Game (StoredGame g) where
