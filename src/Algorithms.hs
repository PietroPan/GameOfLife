module Algorithms where

import Data

type Matriz a = [[a]]

gridSize :: Matriz a -> Pos
gridSize (h:t) = (length h,length (h:t))

validPos :: Pos -> Matriz a -> Bool 
validPos (x,y) m = (x>=0 && x<a) && (y>=0 && y<b)
    where (a,b) = gridSize m

findPosLine :: Int -> [a] -> a
findPosLine n (h:t)
    | n == 0 = h
    | otherwise = findPosLine (n-1) t

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
chagePosLine :: Int -> a -> [a] -> [a]
chagePosLine _ _ [] = []
chagePosLine n a (h:t)
    | n == 0 = (a:t)
    | otherwise = h:chagePosLine (n-1) a t

-- ** Funções sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
findPosGrid :: Pos -> Matriz a -> a
findPosGrid (x,y) m = findPosLine x (findPosLine y m)

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
changePosGrid :: Pos -> a -> Matriz a -> Matriz a
changePosGrid _ _ [] = []
changePosGrid _ _ [[]] = [[]]
changePosGrid (x,y) a (h:t)
    | y==0 = (chagePosLine x a h):t
    | otherwise = h:changePosGrid (x,y-1) a t

nextGrid :: Grid -> Grid
nextGrid g = makeGrid (classicAlg g)

makeLine :: Line -> Line
makeLine [] = []
makeLine ((Cell (Alive age)):xs) = (Cell (Alive (age+1))):makeLine xs
makeLine (x:xs)
    | x == (Cell Born) = (Cell (Alive 0)):makeLine xs
    | x == (Cell Dead) = (Void):makeLine xs
    | otherwise = (x:makeLine xs)

makeGrid :: Grid -> Grid
makeGrid [] = []
makeGrid (x:xs) = (makeLine x):(makeGrid xs)

classicAlg :: Grid -> Grid
classicAlg g = solitude_overPop (fromCells g (0,0) (gridSize g)) (0,0) (gridSize g)

fromCells :: Grid -> Pos -> Pos -> Grid
fromCells g (x,y) pf
    | (x,y) == (-2,-2) = g
    | (validPos (x,y) g) && ((findPosGrid (x,y) g) == Void) && (closeCheck g (x-1,y-1) (x+1,y+1) == 3) = fromCells (changePosGrid (x,y) (Cell Born) g) (newPos' (x,y) pf) pf
    | otherwise = fromCells g (newPos' (x,y) pf) pf

solitude_overPop :: Grid -> Pos -> Pos -> Grid
solitude_overPop g (x,y) pf
    | (x,y) == (-2,-2) = g
    | (validPos (x,y) g) && ((findPosGrid (x,y) g) /= Void) && ((closeCheck g (x-1,y-1) (x+1,y+1) <= 1) || (closeCheck g (x-1,y-1) (x+1,y+1) >= 4)) = solitude_overPop (changePosGrid (x,y) (Cell Dead) g) (newPos' (x,y) pf) pf
    | otherwise = solitude_overPop g (newPos' (x,y) pf) pf

closeCheck :: Grid -> Pos -> Pos -> Int
closeCheck g pi@(xi,yi) pf@(xf,yf)
    | pi == (-2,-2) = 0
    | ((xi/=xf-1) || (yi/=yf-1)) && (validPos pi g) && ((isAlive (findPosGrid pi g)) || (findPosGrid pi g == Cell Dead)) = 1 + closeCheck g (newPos pi pf) pf
    | otherwise = closeCheck g (newPos pi pf) pf

isAlive :: Point -> Bool
isAlive (Cell (Alive _)) = True
isAlive _ = False

newPos' :: Pos -> Pos -> Pos
newPos' (xi,yi) (xf,yf)
    | (yi<0) && (xi<0) = (0,0)
    | (xi<0) = (0,yi)
    | (yi<0) = (xi,0)
    | (xi == xf) && (yi==yf) = (-2,-2)
    | (xi == xf) = (0,yi+1)
    | otherwise = (xi+1,yi)

newPos :: Pos -> Pos -> Pos
newPos (xi,yi) (xf,yf)
    | (xi<0) && (yi<0) = (0,0)
    | (xi<0) = (0,yi)
    | (yi<0) = (xi,0)
    | (xi == xf) && (yi==yf) = (-2,-2)
    | (xi == xf) = (xi-2,yi+1)
    | otherwise = (xi+1,yi)

test :: Grid
test = [[Cell (Alive 0),Cell (Alive 0),Cell (Alive 0),Void,Void]
       ,[Cell (Alive 0),Cell (Alive 0),Cell (Alive 0),Void,Void]
       ,[Cell (Alive 0),Cell (Alive 0),Cell (Alive 0),Void,Void]
       ,[Void,Void,Void,Void,Void]]

test2 :: Grid
test2 = [[Void,Void,Void],[Void,Void,Void],[Void,Void,Void]]