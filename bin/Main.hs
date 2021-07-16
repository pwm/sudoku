module Main where

import Data.Foldable (traverse_)
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
  case traverse parse (lines puzzleFile) of
    Nothing -> die $ "Invalid puzzle file " <> fn
    Just puzzles -> traverse_ (putStrLn . pp . solve) puzzles
