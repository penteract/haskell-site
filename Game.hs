module Game(Player(..),other,Game(..),MetaData(..),Status(..),newMD,
    PlayerID,GameID,gameURL,move, updateMsg) where

--import Data.Aeson(ToJSON(..),Value,object)
import Text.JSON
import Data.Text(pack)
--import Data.Aeson.Types(Pair)
import Data.Bits((.&.))
--import Control.Monad.State
import Network.WebSockets(Connection)
import Data.ByteString.Char8(ByteString,unpack)
import Data.Int
import System.Posix.Time
import Utils
import Control.Arrow

class Eq g => Game g where -- never forget that the implication is the wrong way round
    --makeMove assumes the move is made by the player whose turn it is
    makeMove :: String -> g -> Status -> Either String (g,Status)
    getData :: g -> JSValue
    newGame :: g
    ais :: String -> Maybe (g -> Either String g)

move :: Game g => Player -> String -> (g,MetaData) -> Either String (g,MetaData)
move pl pos (gm,md) =
    second (\s ->(md{status=s}) ) <$> case status md of
        s@(IsTurn p) -> if p==pl then makeMove pos gm s
            else Left "It is not your turn"
        s -> if gameover s then Left "The game has finished"
            else Left "You are trying to make a move for a game that has not started"
     --(\(g,s) -> OXG g (md{status=s}) ) <$> makeMove pos game (status md)

type PlayerID = ByteString
type GameID = ByteString

type GameData g = (g,MetaData)

data MetaData = MD { --data common to all game types
    player0 :: PlayerID,
    player1 :: PlayerID,
    --listeners :: [Connection],
    gid :: GameID,
    lastMove :: Int,--time of last move played
    status :: Status} deriving (Eq) --OK, turn isn't really metadata

newMD :: PlayerID -> PlayerID -> Player -> GameID -> IO MetaData
newMD pl0 pl1 p gid = do
    t <- fromEnum <$> epochTime --systemSeconds<$>getSystemTime
    return MD{
        player0   = pl0,
        player1   = pl1,
        --listeners = [],
        gid       = gid,
        lastMove  = t,
        status    = Unstarted p}

--instance ToJSON ByteString where
--    toJSON b = toJSON $ unpack b

--(.:) :: ToJSON b => String->b->Pair
--a .: b = (pack a,toJSON b)


--(.::) :: ToJSON b => String->b->Pair
--a .:: b = (pack a,toJSON $ unpack b)

updateMsg :: Game g => g -> MetaData -> [(String,JSValue)]
updateMsg gg dat  = --toJSObject
    ["request" .: "update",
     "data" .: (getData  gg),
     "gameID" .: gid dat,
     "state" .: fromEnum (status dat)]

-- player Zero always starts
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
gameURL (MD{gid=n,player0=pl0,player1=pl1}) gname pl = "/{}/play?gameID={}"%gname%unpack n ++ case pl of
    Just p -> "&playerID={}"%unpack ([pl0,pl1]!!fromEnum p)
    Nothing -> ""

--instance Game g => Game (StoredGame g) where
