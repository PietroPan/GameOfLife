module Data where

type Grid = [Line]
type Line = [Point]

data Point
        = Cell Type
        | Void
    deriving (Read,Show,Eq)

type Age = Int

data Type
        = Alive Age
        | Born
        | Dead
    deriving (Read,Show,Eq)

type Pos = (Int,Int)