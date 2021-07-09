module Sudoku.Parser where

import Data.Bifunctor
import qualified Data.Map.Strict as Map
import Sudoku.Types (Grid, Matrix)
import Prelude

parse :: String -> Maybe Grid
parse = fmap matrixToGrid . traverse stringToDigits . lines

stringToDigits :: String -> Maybe [Int]
stringToDigits s =
  let xs = concatMap (fmap fst . (\c -> reads [c])) s
   in if length xs == length s then Just xs else Nothing

matrixToGrid :: Matrix -> Grid
matrixToGrid =
  Map.fromList
    . concatMap (\(x, l) -> fmap (first (x,)) l)
    . zip [0 ..]
    . fmap (zip [0 ..])
