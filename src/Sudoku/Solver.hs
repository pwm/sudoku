module Sudoku.Solver where

import Control.Applicative (Alternative)
import Control.Monad.Logic (observe)
import Data.Foldable (asum)
import Data.List ((\\))
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Sudoku.Types (Grid, Pos)
import Prelude

solve :: Grid -> Grid
solve = observe . go
  where
    go :: (Monad m, Alternative m) => Grid -> m Grid
    go grid
      | done grid = pure grid
      | otherwise = do
        let nextPos = nextHole grid
        choice <- choose (candidates grid nextPos)
        go (Map.insert nextPos choice grid)

done :: Grid -> Bool
done = Map.null . Map.filter (== 0)

nextHole :: Grid -> Pos
nextHole = fst . Map.findMin . Map.filter (== 0)

candidates :: Grid -> Pos -> [Int]
candidates grid pos = [1 .. 9] \\ (rowVals <> colVals <> boxVals)
  where
    rowVals = (grid !) <$> rowOf pos
    colVals = (grid !) <$> colOf pos
    boxVals = (grid !) <$> boxOf pos

choose :: (Traversable t, Alternative m) => t a -> m a
choose = asum . fmap pure

rowOf, colOf, boxOf :: Pos -> [Pos]
rowOf (i, _) = [(i, y) | y <- [0 .. 8]]
colOf (_, j) = [(x, j) | x <- [0 .. 8]]
boxOf (i, j) =
  [ (i - (i `rem` 3) + x, j - (j `rem` 3) + y)
    | x <- [0, 1, 2],
      y <- [0, 1, 2]
  ]
