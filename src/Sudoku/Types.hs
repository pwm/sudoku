module Sudoku.Types where

import Data.Map.Strict (Map)
import Prelude

type Matrix = [[Int]]

type Pos = (Int, Int)

type Grid = Map Pos Int
