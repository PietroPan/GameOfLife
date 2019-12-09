module Grids where

import Data
import Algorithms

makeVoidGrid :: Pos -> Grid
makeVoidGrid (x,0) = []
makeVoidGrid (x,y) = makeVoidLine x:makeVoidGrid (x,y-1)

makeVoidLine :: Int -> Line
makeVoidLine 0 = []
makeVoidLine x = Void:makeVoidLine(x-1)

void :: Grid
void = makeVoidGrid (70,50)

tumbler :: Grid
tumbler = changeVPosGrid (makeVoidGrid (70,50)) [(32,22),(33,22),(35,22),(36,22),(32,23),(33,23),(35,23),(36,23),(33,24),(35,24),(31,25),(33,25),(35,25),(37,25),(31,26),(33,26),(35,26),(37,26),(31,27),(32,27),(36,27),(37,27)]

spaceship :: Grid
spaceship = changeVPosGrid (makeVoidGrid (70,50)) [(33,22),(34,22),(35,22),(36,22),(32,23),(36,23),(36,24),(32,25),(35,25)]

cellrow :: Grid
cellrow = changeVPosGrid (makeVoidGrid (70,50)) [(30,25),(31,25),(32,25),(33,25),(34,25),(35,25),(36,25),(37,25),(38,25),(39,25)]

exploder :: Grid
exploder = changeVPosGrid (makeVoidGrid (70,50)) [(33,23),(35,23),(37,23),(33,24),(33,25),(33,26),(33,27),(35,27),(37,27),(37,25),(37,26),(37,24)]

smallexploder :: Grid
smallexploder = changeVPosGrid (makeVoidGrid (70,50)) [(35,24),(35,23),(34,24),(36,24),(34,25),(36,25),(35,26)]

glider :: Grid
glider = changeVPosGrid (makeVoidGrid (70,50)) [(34,23),(35,24),(33,25),(34,25),(35,25)]

changeVPosGrid :: Grid -> [Pos] -> Grid
changeVPosGrid g [] = g
changeVPosGrid g (x:xs) = changeVPosGrid (changePosGrid x (Cell (Alive 0)) g) xs