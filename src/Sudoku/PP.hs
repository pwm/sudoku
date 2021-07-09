module Sudoku.PP where

import Data.List (foldl')
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Sudoku.Types (Grid, Matrix)
import Prelude

pp :: Grid -> String
pp = drawMatrix . gridToMatrix

gridToMatrix :: Grid -> Matrix
gridToMatrix = chunksOf 9 . Map.elems

drawMatrix :: Matrix -> String
drawMatrix = foldl' (\s xs -> foldl' (\s' v -> s' <> show v) s xs <> "\n") ""
