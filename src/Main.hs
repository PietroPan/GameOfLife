module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Data.Maybe
import Data.List
import Data.Char

import Data
import Algorithms
import Grids

type MainState = (Grid,Float,[Picture])

loadIMG :: IO[Picture]
loadIMG = return[]

testc :: Picture
testc = (Color red (Polygon [(0,0),(1,0),(1,1),(0,1)]))

grid :: Picture
grid = Color black (Polygon [(0,0),(1,0),(1,1),(0,1)])

getcell :: Int -> Picture
getcell age = Color (darker age white) (Polygon [(0,0),(1,0),(1,1),(0,1)])

darker :: Int -> Color -> Color
darker 0 c = c
darker age c = darker (age-1) (dark c)

disM :: Display
disM = InWindow "GoL" (1000,700) (0,0)
--disM = FullScreen

inicialState :: [Picture] -> MainState
inicialState loadedIMG = (void,1,loadedIMG)

getPos :: (Float,Float) -> Pos
getPos (a,b) = (truncate ((a+400)/12),(truncate (abs((b-306)/12))))

changeCell :: Pointt -> Pointt
changeCell Void = (Cell (Alive 0))
changeCell (Cell _) = Void

eventChange :: Event -> MainState -> MainState
eventChange (EventKey a Down _ (x,y)) s@(g,n,pics) = case a of (Char 'p') | (n>=1) -> (g,(-n),pics)
                                                                          | (n<2) -> (g,(-n),pics)
                                                                          | (n==(-3)) -> (g,3,pics)
                                                                          | otherwise -> (g,n,pics)
                                                               (SpecialKey KeyRight) | (abs(n)==1) -> (glider,1.1,pics)
                                                                                     | (abs(n)==1.1) -> (smallexploder,1.2,pics)
                                                                                     | (abs(n)==1.2) -> (exploder,1.3,pics)
                                                                                     | (abs(n)==1.3) -> (cellrow,1.4,pics)
                                                                                     | (abs(n)==1.4) -> (spaceship,1.5,pics)
                                                                                     | (abs(n)==1.5) -> (tumbler,1.6,pics)
                                                                                     | otherwise -> (void,1,pics)
                                                               (SpecialKey KeyLeft) | (abs(n)==1) -> (tumbler,1.6,pics)
                                                                                    | otherwise -> (void,1,pics)
                                                                                    | (abs(n)==1.2) -> (glider,1.1,pics)
                                                                                    | (abs(n)==1.3) -> (smallexploder,1.2,pics)
                                                                                    | (abs(n)==1.4) -> (exploder,1.3,pics)
                                                                                    | (abs(n)==1.5) -> (cellrow,1.4,pics)
                                                                                    | (abs(n)==1.6) -> (spaceship,1.5,pics)
                                                               (Char 'e') -> (void,(-3),pics)
                                                               (MouseButton LeftButton) | (n==(-3)) -> ((changePosGrid (getPos (x,y)) (changeCell (findPosGrid (getPos (x,y)) g)) g),n,pics)
                                                               _ -> (g,n,pics)
eventChange _ s = s

timeChange :: Float -> MainState -> MainState
timeChange f s@(g,n,pics) | (n>0) = (nextGrid g,n,pics)
                          | otherwise = (g,n,pics)

drawtest :: MainState -> Picture
drawtest s@(g,n,pics) = pictures ((drawState s):(Translate (-382) (300) testc):[])

drawState :: MainState -> Picture
--drawState s@(g,f,pics) = rotate (-180) (scale 3 3 (Pictures(drawGrid s (0,0))))
drawState s@(g,n,pics) = Translate (-400) (-300) (scale 6 6 (Pictures(drawGrid s (0,2*snd(gridSize g)))))

-- (-400, 306) (-394, 306) | (-388, 306) (-382, 306)
-- (-400, 300) (-394, 300) | (-388, 300) (-382, 300)
get :: Age -> Int
get age | (age > 6) = 7
        | otherwise = age+1

drawGrid :: MainState -> Pos -> [Picture]
drawGrid ([],f,pics) (x,y) = []
drawGrid (([]:ts),f,pics) (x,y) = drawGrid (ts,f,pics) (0,y-2)
drawGrid s@(((Void:t):ts),f,pics) (x,y) = (Translate (fromIntegral x) (fromIntegral y) grid) : drawGrid ((t:ts),f,pics) (x+2,y)
drawGrid s@(((Cell(Alive age):t):ts),f,pics) (x,y) = (Translate (fromIntegral x) (fromIntegral y) (getcell age)) : drawGrid ((t:ts),f,pics) (x+2,y)

main :: IO()
main = do loadedIMG <- loadIMG
          play disM
               (greyN 0.2)
               6
               (inicialState loadedIMG)
               drawtest
               eventChange
               timeChange

