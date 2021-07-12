module Sudoku.Solver where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Control.Monad.Logic (Logic, observe)
import Data.Foldable (asum)
import Data.List ((\\))
import Data.List.Extra (nubOrd)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Sudoku.Types (Grid, Pos)
import Prelude

solve :: Grid -> Grid
solve = observe . go
  where
    go :: Grid -> Logic Grid
    go grid = do
      guard (rules grid)
      if done grid
        then pure grid
        else do
          choice <- choose (candidates grid)
          go (add choice grid)

rules :: Grid -> Bool
rules grid = validate rows && validate cols && validate boxes
  where
    validate :: [[Pos]] -> Bool
    validate = and . fmap (valsValid . valsAt grid)
    valsValid :: [Int] -> Bool
    valsValid vs = 0 `elem` vs || length vs == length (nubOrd vs)

done :: Grid -> Bool
done = Map.null . Map.filter (== 0)

choose :: [a] -> Logic a
choose = asum . fmap pure

candidates :: Grid -> [Int]
candidates grid = [1 .. 9] \\ (rowVals <> colVals <> boxVals)
  where
    rowVals = valsAt grid (rowOf (nextHole grid))
    colVals = valsAt grid (colOf (nextHole grid))
    boxVals = valsAt grid (boxOf (nextHole grid))

add :: Int -> Grid -> Grid
add v grid = Map.insert (nextHole grid) v grid

nextHole :: Grid -> Pos
nextHole = fst . Map.findMin . Map.filter (== 0)

rows, cols, boxes :: [[Pos]]
rows = rowOf <$> zip [0 .. 8] [0 ..]
cols = colOf <$> zip [0 ..] [0 .. 8]
boxes = boxOf <$> liftA2 (,) [0, 3, 6] [0, 3, 6]

rowOf, colOf, boxOf :: Pos -> [Pos]
rowOf (i, _) = [(i, y) | y <- [0 .. 8]]
colOf (_, j) = [(x, j) | x <- [0 .. 8]]
boxOf (i, j) =
  [ (i - (i `rem` 3) + x, j - (j `rem` 3) + y)
    | x <- [0, 1, 2],
      y <- [0, 1, 2]
  ]

valsAt :: Grid -> [Pos] -> [Int]
valsAt grid = fmap (grid !)
