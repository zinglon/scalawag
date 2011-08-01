module Game where

import Data.Array

import Core
import Vec
import Util

data Terrain = Dirt | Wall deriving (Eq, Ord, Show, Read)

data Level = Level { size    :: Int
                   , terrain :: Array (Pt2 Int) Terrain
                   } deriving (Eq, Ord, Show, Read)

data Game = Game { player :: Pt2 Int
                 , level  :: Level 
                 } deriving (Eq, Ord, Show, Read)

newGame :: Game
newGame = Game 0 (emptyLevel 9)

emptyLevel :: Int -> Level
emptyLevel sz = Level sz (listArray (minPt, maxPt) (cycle $ Wall : replicate (sz - 1) Dirt))
  where minPt = pt2 0 0
        maxPt = pt2 (sz - 1) (sz - 1)

levelSize :: Game -> Int
levelSize = size . level

levelDim :: Game -> Pt2 Int
levelDim = pure . levelSize


