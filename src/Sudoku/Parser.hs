module Sudoku.Parser where

import qualified Data.Map.Strict as Map
import Sudoku.Types (Grid, Matrix, Pos)
import Prelude

parse :: String -> Maybe Grid
parse = fmap matrixToGrid . traverse stringToDigits . lines

stringToDigits :: String -> Maybe [Int]
stringToDigits s =
  let xs = concatMap (fmap fst . (\c -> reads [c])) s
   in if length xs == length s then Just xs else Nothing

matrixToGrid :: Matrix -> Grid
matrixToGrid = Map.fromList . concatMap rowToPos . index
  where
    index :: Matrix -> [(Int, [(Int, Int)])]
    index = zip [0 ..] . fmap (zip [0 ..])
    rowToPos :: (Int, [(Int, Int)]) -> [(Pos, Int)]
    rowToPos (i, xs) = let (js, vs) = unzip xs in zip (zip (replicate 9 i) js) vs
