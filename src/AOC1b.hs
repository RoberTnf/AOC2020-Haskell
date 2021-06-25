module AOC1b where

import AOC
import Data.Bifunctor (Bifunctor (bimap))

-- |
-- >>> productOfFilteredSum ["1721", "979", "366", "299", "675", "1456"]
-- 241861950
productOfFilteredSum :: [String] -> Int
productOfFilteredSum input =
  let filteredInput =
        filter
          (\(a, b, c) -> a + b + c == 2020)
          [ (read a, read b, read c)
            | a <- input,
              b <- input,
              a /= b,
              c <- input,
              a /= c,
              b /= c
          ]
   in (\(a, b, c) -> a * b * c) $ head filteredInput

aoc1b :: IO ()
aoc1b = do
  input <- getInput 1
  print $ productOfFilteredSum input