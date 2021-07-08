module Main where

import Sudoku
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Prelude

main :: IO ()
main = do
  (puzzleFile, fn) <-
    getArgs >>= \case
      [fn] -> (,fn) <$> readFile fn
      _ -> do
        pn <- getProgName
        failWith $ "Usage: " <> pn <> " <puzzle>"
  case parse puzzleFile of
    Nothing -> failWith $ "Invalid puzzle file " <> fn
    Just puzzle -> putStrLn $ pp (solve puzzle)

failWith :: String -> (forall a. IO a)
failWith s = putStrLn s >> exitFailure
