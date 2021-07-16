module Sudoku.PP where

import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Sudoku.Types (Grid)
import Prelude

pp :: Grid -> String
pp = concatMap show . Map.elems

ppM :: Grid -> String
ppM =
  concatMap ((<> "\n") . concatMap show)
    . chunksOf 9
    . Map.elems
