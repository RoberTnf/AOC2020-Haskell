module Main where

import AOC1a
import AOC1b
import AOC2a
import AOC2b
import AOC3a
import AOC3b
import AOC4a
import AOC4b
import System.Environment

main :: IO ()
main = do
  (command : _) <- getArgs
  case command of
    "1a" -> aoc1a
    "1b" -> aoc1b
    "2a" -> aoc2a
    "2b" -> aoc2b
    "3a" -> aoc3a
    "3b" -> aoc3b
    "4a" -> aoc4a
    "4b" -> aoc4b
    x -> error $ "Exercise " ++ x ++ " does not exist"
