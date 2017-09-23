module Simulation (World (..), evolve) where

import Control.Monad.Reader 
import qualified Data.Vector as V

evolve :: Reader World World
evolve = ask >>= \w ->
    liftM (\res -> w {cells = res}) (imapM' (evolveCell) (cells w))

evolveCell :: (Int, Int) -> Bool -> Reader World Bool
evolveCell c b = 
    if b
        then liftM (flip elem [2,3]) (aliveNeighboursCount c)
        else liftM (3 ==) (aliveNeighboursCount c)

aliveNeighboursCount :: (Int, Int) -> Reader World Int
aliveNeighboursCount = liftM length . aliveNeighbours 

aliveNeighbours :: (Int, Int) -> Reader World [(Int, Int)]
aliveNeighbours = (=<<) (filterM isAlive) . neighbours

neighbours :: (Int, Int) -> Reader World [(Int, Int)]
neighbours (x, y) = mapM wrap $ do
    dx <- [-1..1]
    dy <- [-1..1]
    guard (dx /= 0 || dy /= 0)
    return (x + dx, y + dy)

wrap :: (Int, Int) -> Reader World (Int, Int)
wrap (x, y) = liftM (\w -> (x `mod` (width w), y `mod` (height w))) ask

isAlive :: (Int, Int) -> Reader World Bool
isAlive (x, y) = liftM (\w -> ((cells w) V.! x) V.! y) ask

data World 
    = World 
    { width :: Int
    , height :: Int
    , cells :: Matrix Bool} 
    deriving (Show)

type Matrix a = V.Vector (V.Vector a)

imapM' :: ((Int, Int) -> Bool -> Reader World Bool) -> (Matrix Bool) -> Reader World (Matrix Bool)
imapM' f x = sequence $ V.imap (\i v -> sequence $ V.imap (\j e ->(f (i, j) e)) v) x