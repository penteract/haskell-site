module OX where

import Game
import Tools

import Data.Array

type Pos = (Int,Int,Int)
type Line = [Pos]

data OX = OX (Array Pos Char)

toSym One = 'X'
toSym Zero = 'O'

--Status is guarenteed to be of the form (IsTurn p)
move :: Pos -> OX -> Status -> Either String (OX,Status)
move pos (OX board) (IsTurn pl) = do
    "Position not inside grid" `unless` inRange grid pos
    "Cell is not Empty" `unless` (board!pos==' ')

    let tok = toSym pl
    let newBoard = board//[(pos,tok)]
    return (newBoard,
        if any [all [newBoard!x == tok | x<-line] | line <- linesThrough pos]
            then  (newBoard,Won pl)
            else if any [newBoard!x==' ' | x<-grid]
                then IsTurn $ other pl
                else Draw)



instance Game OX where
    newGame = OX $ array grid [(i,' ') | i<-range grid]
    makeMove val g s = error ""



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
linesThrough (0,0,0) = [[mapt (step*) (dx,dy,dz) |step<-[0..3]] | (dx,dy,dz) <- (tail $ range ((0,0,0),(1,1,1)))]
linesThrough (0,0,1) = [(0,0,step) | step<-[0..3]]:[ [(step*dx,step*dy,1) | step <- [0..3]] | (dx,dy) <- tail $ range ((0,0),(1,1))]

linesThrough p = map (map t) $ linesThrough (t p) where
    t = trans p
