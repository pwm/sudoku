module Sudoku.Solver where

import Control.Applicative (Alternative)
import Control.Monad (guard)
import Control.Monad.Logic (observe)
import Data.Foldable (asum)
import Data.List ((\\))
import Data.List.Extra (nubOrd)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Sudoku.Types (Grid, Pos)
import Prelude

solve :: Grid -> Grid
solve = observe . go (0, 0)
  where
    go :: (Monad m, Alternative m) => Pos -> Grid -> m Grid
    go curPos grid = do
      guard (rules grid curPos)
      if done grid
        then pure grid
        else do
          let nextPos = nextHole grid
          choice <- choose (candidates grid nextPos)
          go nextPos (Map.insert nextPos choice grid)

rules :: Grid -> Pos -> Bool
rules grid pos =
  valid (rowOf pos) && valid (colOf pos) && valid (boxOf pos)
  where
    valid :: [Pos] -> Bool
    valid = valsValid . valsAt grid
    valsValid :: [Int] -> Bool
    valsValid vs = 0 `elem` vs || 9 == length (nubOrd vs)

done :: Grid -> Bool
done = Map.null . Map.filter (== 0)

nextHole :: Grid -> Pos
nextHole = fst . Map.findMin . Map.filter (== 0)

candidates :: Grid -> Pos -> [Int]
candidates grid pos = [1 .. 9] \\ (rowVals <> colVals <> boxVals)
  where
    rowVals = valsAt grid (rowOf pos)
    colVals = valsAt grid (colOf pos)
    boxVals = valsAt grid (boxOf pos)

valsAt :: Grid -> [Pos] -> [Int]
valsAt grid = fmap (grid !)

choose :: Alternative m => [a] -> m a
choose = asum . fmap pure

rowOf, colOf, boxOf :: Pos -> [Pos]
rowOf (i, _) = [(i, y) | y <- [0 .. 8]]
colOf (_, j) = [(x, j) | x <- [0 .. 8]]
boxOf (i, j) =
  [ (i - (i `rem` 3) + x, j - (j `rem` 3) + y)
    | x <- [0, 1, 2],
      y <- [0, 1, 2]
  ]
