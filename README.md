# Sudoku

[![CI](https://github.com/pwm/sudoku/workflows/CI/badge.svg)](https://github.com/pwm/sudoku/actions)

A simple Sudoku solver using [LogicT](https://hackage.haskell.org/package/logict).

Build / run / test:
```
cabal build
cabal run sudoku test/golden/puzzles.txt
cabal test --test-show-details=direct
```
