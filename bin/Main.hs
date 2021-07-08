module Main where

import Sudoku.Solver
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Prelude

main :: IO ()
main = do
  (puzzleFile, fn) <-
    getArgs >>= \case
      [] -> do
        pn <- getProgName
        putStrLn $ "Usage: " <> pn <> " <puzzle>"
        putStrLn $ "  eg.: " <> pn <> " puzzles/0.txt"
        exitFailure
      (fn : _) -> (,fn) <$> readFile fn
  case parse puzzleFile of
    Nothing -> do
      putStrLn $ fn <> " is not a valid sudoku puzzle file."
      exitFailure
    Just puzzle -> pp $ solve puzzle
