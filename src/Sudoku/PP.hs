module Sudoku.PP (pp) where

import Data.List (foldl')
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Sudoku.Types (Grid, Matrix)
import Prelude

pp :: Grid -> IO ()
pp = putStrLn . draw . toMatrix

toMatrix :: Grid -> Matrix
toMatrix = chunksOf 9 . Map.elems

draw :: Matrix -> String
draw = foldl' (\s xs -> foldl' (\s' v -> s' <> show v) s xs <> "\n") ""
