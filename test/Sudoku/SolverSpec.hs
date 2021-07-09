module Sudoku.SolverSpec where

import Paths_sudoku (getDataDir)
import Sudoku
import System.Directory (listDirectory)
import System.FilePath (normalise, (</>))
import Test.Hspec
import Prelude

spec :: Spec
spec =
  describe "Golden tests" $ do
    it "Solver solutions matches golden solutions" $ do
      puzzles <- loadFilesFromDir "test/golden/puzzles"
      solutions <- loadFilesFromDir "test/golden/solutions"
      zip puzzles solutions `shouldSatisfy` solvePuzzles

loadFilesFromDir :: FilePath -> IO [String]
loadFilesFromDir relDir = do
  dataDir <- getDataDir
  let absDir = normalise (dataDir </> relDir)
  absFiles <- fmap (absDir </>) <$> listDirectory absDir
  traverse readFile absFiles

solvePuzzles :: [(String, String)] -> Bool
solvePuzzles = and . fmap solvePuzzle
  where
    solvePuzzle :: (String, String) -> Bool
    solvePuzzle (p, s) = (fmap (pp . solve) . parse) p == Just s
