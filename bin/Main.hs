module Main where

import Sudoku
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Prelude

main :: IO ()
main = do
  (puzzleFile, fn) <-
    getArgs >>= \case
      [fn] -> (,fn) <$> readFile fn
      _ -> getProgName >>= \pn -> die $ "Usage: " <> pn <> " <file>"
  case parse puzzleFile of
    Nothing -> die $ "Invalid puzzle file " <> fn
    Just puzzle -> putStrLn $ pp (solve puzzle)
