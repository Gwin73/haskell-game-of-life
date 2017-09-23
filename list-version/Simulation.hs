module Simulation (World (..), nextGen) where

import Control.Monad.Reader 
import Data.List (nub)

nextGen :: Reader World World
nextGen = do
    world <- ask
    newAliceCells <- liftM2 (++) survivors births
    return $ world {aliveCells = newAliceCells}

births :: Reader World [(Int, Int)] 
births = ask >>= mapM neighbours . aliveCells >>= filterM p . nub . concat
    where p = liftM (==3) . aliveNeighboursCount

survivors :: Reader World [(Int, Int)]
survivors = filterM p . aliveCells =<< ask
    where p = liftM (flip elem [2, 3]) . aliveNeighboursCount 

aliveNeighboursCount :: (Int, Int) -> Reader World Int
aliveNeighboursCount =
    liftM length . aliveNeighbours 

aliveNeighbours :: (Int, Int) -> Reader World [(Int, Int)]
aliveNeighbours = (=<<) (filterM isAlive) . neighbours

neighbours :: (Int, Int) -> Reader World [(Int, Int)]
neighbours (x, y) = mapM wrap (do
    dx <- [-1..1]
    dy <- [-1..1]
    guard (dx /= 0 || dy /= 0)
    return (x + dx, y + dy))

wrap :: (Int, Int) -> Reader World (Int, Int)
wrap (x, y) = liftM (\w -> (((x-1) `mod` (width w)) + 1, ((y-1) `mod` (height w)) + 1)) ask

isAlive :: (Int, Int) -> Reader World Bool
isAlive cell = liftM (elem cell . aliveCells) ask 

data World 
    = World 
    { width :: Int
    , height :: Int
    , aliveCells :: [(Int, Int)]} 
    deriving (Show)