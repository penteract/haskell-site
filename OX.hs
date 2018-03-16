module OX where

import Game
import Tools

import Data.Array
import Data.Char(ord)
import Data.Maybe(fromMaybe)

import Data.Aeson(toJSON)

type Pos = (Int,Int,Int)
type Line = [Pos]

newtype OX = OX (Array Pos Char)


toSym One = 'X'
toSym Zero = 'O'

--Status is guarenteed to be of the form (IsTurn p)
move :: Pos -> OX -> Status -> Either String (OX,Status)
move pos (OX board) (IsTurn pl) = do
    "Position not inside grid" `unless` inRange grid pos
    "Cell is not Empty" `unless` (board!pos==' ')

    let tok = toSym pl
    let newBoard = board//[(pos,tok)]
    return (OX newBoard,
        if any (all (\x -> newBoard!x == tok)) (linesThrough pos)
            then Won pl
            else if any (\x -> newBoard!x==' ') grid
                then IsTurn $ other pl
                else Draw)



instance Game OX where
    newGame = OX $ array grid [(i,' ') | i<-range grid]
    makeMove pos g s =
        getPos pos >>= (\ p -> move p g s)
    getData (OX arr) = toJSON $ elems arr
    ais = lookIn []

getPos :: String -> Either String (Int,Int,Int)
getPos pos = fromMaybe (Left "Bad position format") (do
    ns <- mapM getCor pos
    case ns of
        [x,y,z] -> return $ Right (x,y,z)
        _ -> Nothing)

getCor :: Char -> Maybe Int
getCor c = if n>=0 && n<4 then return n else Nothing
    where n = ord c - 32


grid :: ((Int,Int,Int),(Int,Int,Int))
grid = ((0,0,0),(3,3,3))

--returns a self inverse, line preserving function which is a step towards mapping pos to (0,0,0) or (0,0,1)

trans :: Pos->Pos->Pos
trans pos@(x,y,z)
    | any (>1) [x,y,z] = zipt (\ x -> if x>1 then (3-) else id) pos
    | x+y+z>1   = mapt (\x->x+1-(2*(x`mod`2)))
    | otherwise = \(a,b,c)-> (a-a*x+c*x,b-b*y+c*y,c*(1-x-y)+a*x+b*y)

mapt :: (Int->Int) -> Pos -> Pos
mapt f (a,b,c) = (f a, f b, f c)

zipt :: (Int->Int->Int) -> Pos -> Pos -> Pos
zipt f (a,b,c) (x,y,z) = (f a x, f b y, f c z)

-- Consider all nonempty subsets of {x,y,z}
-- each defines a line through 0,0,0 indicating the changing coordinate

linesThrough :: Pos -> [Line]
linesThrough (0,0,0) = [ [mapt (step*) (dx,dy,dz) | step<-[0..3]]
    | (dx,dy,dz) <- tail $ range ((0,0,0),(1,1,1))]
linesThrough (0,0,1) = [(0,0,step) | step<-[0..3]]:
  [ [(step*dx,step*dy,1) | step <- [0..3]]
    | (dx,dy) <- tail $ range ((0,0),(1,1))]

linesThrough p = map (map t) $ linesThrough (t p)
    where t = trans p
