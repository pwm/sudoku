module Sudoku.Solver
  ( parse,
    solveSudoku,
    pp,
  )
where

import Control.Monad.Logic (Logic, guard, observe)
import Data.Foldable (asum)
import Data.List (foldl', (\\))
import Data.List.Extra (nubOrd)
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Prelude

type Matrix = [[Int]]

type Pos = (Int, Int)

type Grid = Map Pos Int

solveSudoku :: Matrix -> Matrix
solveSudoku = toMatrix . observe . solve . toGrid
  where
    solve :: Grid -> Logic Grid
    solve g = do
      guard (rules g)
      if done g
        then pure g
        else do
          c <- choose (choices g)
          solve (addChoice g c)

rules :: Grid -> Bool
rules g = validate rows && validate cols && validate boxes
  where
    validate :: [[Pos]] -> Bool
    validate = and . fmap (valid . valsAt g)
    valid :: [Int] -> Bool
    valid xs
      | 0 `elem` xs = True
      | otherwise = length xs == length (nubOrd xs)

done :: Grid -> Bool
done = Map.null . Map.filter (== 0)

choose :: [a] -> Logic a
choose = asum . fmap pure

choices :: Grid -> [Int]
choices g = [1 .. 9] \\ (vals (rowOf p) <> vals (colOf p) <> vals (boxOf p))
  where
    p = nextHole g
    vals = valsAt g

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

-- Parse/Transform

parse :: String -> Maybe Matrix
parse = traverse stringToDigits . lines
  where
    stringToDigits :: String -> Maybe [Int]
    stringToDigits s =
      let xs = concatMap (fmap fst . (\c -> reads [c])) s
       in if length xs == length s then Just xs else Nothing

toGrid :: Matrix -> Grid
toGrid = Map.fromList . concatMap rowToPos . index
  where
    index :: Matrix -> [(Int, [(Int, Int)])]
    index = zip [0 ..] . fmap (zip [0 ..])
    rowToPos :: (Int, [(Int, Int)]) -> [(Pos, Int)]
    rowToPos (i, xs) = let (js, vs) = unzip xs in zip (zip (replicate 9 i) js) vs

toMatrix :: Grid -> Matrix
toMatrix = chunksOf 9 . Map.elems

-- Pretty Print

pp :: Matrix -> IO ()
pp = putStrLn . drawMatrix

drawMatrix :: Matrix -> String
drawMatrix = foldl' (\s xs -> foldl' (\s' v -> s' <> show v) s xs <> "\n") ""
