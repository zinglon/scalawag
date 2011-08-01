module Game where

import Core
import Vec
import Util
                 
data Terrain = Dirt | Wall deriving (Eq, Ord, Show, Read)

data Level = Level { size    :: Int
                   , terrain :: [[Terrain]] 
                   } deriving (Eq, Ord, Show, Read)

data Game = Game { player :: Pt2 Int
                 , level  :: Level 
                 } deriving (Eq, Ord, Show, Read)

newGame :: Game
newGame = Game 0 (emptyLevel 9)

emptyLevel :: Int -> Level
emptyLevel sz = Level sz $ (replicate sz Wall) : (replicate (sz-1) (replicate sz Dirt))

levelSize :: Game -> Int
levelSize = size . level

levelDim :: Game -> Pt2 Int
levelDim = pure . levelSize


