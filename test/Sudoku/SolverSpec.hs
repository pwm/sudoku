module Sudoku.SolverSpec where

import Paths_sudoku (getDataFileName)
import Sudoku
import Test.Hspec
import Prelude

spec :: Spec
spec =
  describe "Golden tests" $ do
    it "Solver solutions matches golden solutions" $ do
      puzzles <- fmap lines $ readFile =<< getDataFileName "test/golden/puzzles.txt"
      solutions <- fmap lines $ readFile =<< getDataFileName "test/golden/solutions.txt"
      zip puzzles solutions `shouldSatisfy` (and . fmap solutionMatchGolden)

solutionMatchGolden :: (String, String) -> Bool
solutionMatchGolden (puzzle, golden) = (fmap (pp . solve) . parse) puzzle == Just golden
