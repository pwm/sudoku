module Sudoku.Solver (solve) where

import Control.Monad.Logic (Logic, guard, observe)
import Data.Foldable (asum)
import Data.List ((\\))
import Data.List.Extra (nubOrd)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Sudoku.Types
import Prelude

solve :: Grid -> Grid
solve = observe . go
  where
    go :: Grid -> Logic Grid
    go g = do
      guard (rules g)
      if done g
        then pure g
        else do
          c <- choose (choices g)
          go (addChoice g c)

rules :: Grid -> Bool
rules g = validate rows && validate cols && validate boxes
  where
    validate :: [[Pos]] -> Bool
    validate = and . fmap (valid . valsAt g)
    valid :: [Int] -> Bool
    valid xs = 0 `elem` xs || length xs == length (nubOrd xs)

done :: Grid -> Bool
done = Map.null . Map.filter (== 0)

choose :: [a] -> Logic a
choose = asum . fmap pure

choices :: Grid -> [Int]
choices g =
  [1 .. 9] \\ (vals (rowOf p) <> vals (colOf p) <> vals (boxOf p))
  where
    vals = valsAt g
    p = nextHole g

addChoice :: Grid -> Int -> Grid
addChoice g c = Map.insert (nextHole g) c g

nextHole :: Grid -> Pos
nextHole = fst . Map.findMin . Map.filter (== 0)

rows, cols, boxes :: [[Pos]]
rows = rowOf <$> zip [0 .. 8] [0 .. 8]
cols = colOf <$> zip [0 .. 8] [0 .. 8]
boxes = boxOf <$> zip [0 .. 8] [0 .. 8]

rowOf, colOf, boxOf :: Pos -> [Pos]
rowOf (i, _) = [(i, y) | y <- [0 .. 8]]
colOf (_, j) = [(x, j) | x <- [0 .. 8]]
boxOf (i, j) = [(i + x, j + y) | x <- relTo i, y <- relTo j]
  where
    relTo a = subtract (a `rem` 3) <$> [0, 1, 2]

valsAt :: Grid -> [Pos] -> [Int]
valsAt g = fmap (g !)
