module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
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
loadIMG = do void <- loadJuicy "Images/Void.png" --0
             cell0 <- loadJuicy "Images/Cell.png" --1
             cell1 <- loadJuicy "Images/Cell1.png" --2
             cell2 <- loadJuicy "Images/Cell2.png" --3
             cell3 <- loadJuicy "Images/Cell3.png" --4
             cell4 <- loadJuicy "Images/Cell4.png" --5
             cell5 <- loadJuicy "Images/Cell5.png" --6
             cell6 <- loadJuicy "Images/Cell6.png" --7
             return (map fromJust [void,cell0,cell1,cell2,cell3,cell4,cell5,cell6])

disM :: Display
disM = InWindow "GoL" (1920,1080) (0,0)
--disM = FullScreen

inicialState :: [Picture] -> MainState
--inicialState loadedIMG = ((makeVoidGrid (70,50)),0,loadedIMG)
inicialState loadedIMG = (void,1,loadedIMG)
--inicialState loadedIMG = (void,0,loadedIMG)

eventChange :: Event -> MainState -> MainState
eventChange (EventKey a Down _ _) s@(g,n,pics) = case a of (Char 'p') | (n/=2) -> (g,2,pics)
                                                                      | otherwise -> (g,1,pics)
                                                           (SpecialKey KeyRight) | (n==1) -> (glider,1.1,pics)
                                                                                 | (n==1.1) -> (smallexploder,1.2,pics)
                                                                                 | (n==1.2) -> (exploder,1.3,pics)
                                                                                 | (n==1.3) -> (cellrow,1.4,pics)
                                                                                 | (n==1.4) -> (spaceship,1.5,pics)
                                                                                 | (n==1.5) -> (tumbler,1.6,pics)
                                                                                 | (n==1.6) -> (void,1,pics)
                                                           (SpecialKey KeyLeft) | (n==1) -> (tumbler,1.6,pics)
                                                                                | (n==1.1) -> (void,1,pics)
                                                                                | (n==1.2) -> (glider,1.1,pics)
                                                                                | (n==1.3) -> (smallexploder,1.2,pics)
                                                                                | (n==1.4) -> (exploder,1.3,pics)
                                                                                | (n==1.5) -> (cellrow,1.4,pics)
                                                                                | (n==1.6) -> (spaceship,1.5,pics)
                                                           _ -> (g,n,pics)
eventChange _ s = s

timeChange :: Float -> MainState -> MainState
timeChange f s@(g,2,pics) = (g,2,pics) 
timeChange f s@(g,n,pics) = (nextGrid g,n,pics)

drawState :: MainState -> Picture
--drawState s@(g,f,pics) = rotate (-180) (scale 3 3 (Pictures(drawGrid s (0,0))))
drawState s@(g,n,pics) = Translate (-400) (-300) (scale 6 6 (Pictures(drawGrid s (0,2*snd(gridSize g)))))

get :: Age -> Int
get age | (age > 6) = 7
        | otherwise = age+1

drawGrid :: MainState -> Pos -> [Picture]
drawGrid ([],f,pics) (x,y) = []
drawGrid (([]:ts),f,pics) (x,y) = drawGrid (ts,f,pics) (0,y-2)
drawGrid s@(((Void:t):ts),f,pics) (x,y) = (Translate (fromIntegral x) (fromIntegral y) (pics !! 0)) : drawGrid ((t:ts),f,pics) (x+2,y)
drawGrid s@(((Cell(Alive age):t):ts),f,pics) (x,y) = (Translate (fromIntegral x) (fromIntegral y) (pics !! (get age))) : drawGrid ((t:ts),f,pics) (x+2,y)

main :: IO()
main = do loadedIMG <- loadIMG
          play disM
               (greyN 0.2)
               6
               (inicialState loadedIMG)
               drawState
               eventChange
               timeChange
