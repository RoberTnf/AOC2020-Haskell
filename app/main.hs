module Main where

import AOC1a
import AOC1b
import AOC2a
import AOC2b
import AOC3a
import AOC3b
import AOC4a
import AOC4b
import AOC5a
import AOC5b
import AOC6a
import AOC6b
import AOC7a
import AOC7b
import AOC8a
import AOC8b
import AOC9a
import AOC9b
import AOC10a
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
    "5a" -> aoc5a
    "5b" -> aoc5b
    "6a" -> aoc6a
    "6b" -> aoc6b
    "7a" -> aoc7a
    "7b" -> aoc7b
    "8a" -> aoc8a
    "8b" -> aoc8b
    "9a" -> aoc9a
    "9b" -> aoc9b
    "10a" -> aoc10a
    x -> error $ "Exercise " ++ x ++ " does not exist"
