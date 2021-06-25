module AOC1b where

import AOC
import Control.Monad
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
              c <- input
          ]
   in (\(a, b, c) -> a * b * c) $ head filteredInput

-- Kind of like the do approach more. Seems clearer, and no lambda functions.

-- |
-- >>> productOfFilteredSumDo ["1721", "979", "366", "299", "675", "1456"]
-- 241861950
productOfFilteredSumDo :: [String] -> Int
productOfFilteredSumDo input =
  let x = do
        x_ <- input
        y_ <- input
        z_ <- input
        let (x, y, z) = (read x_, read y_, read z_)
        guard (x + y + z == 2020)
        return (x * y * z)
   in head x

aoc1b :: IO ()
aoc1b = do
  input <- getInput 1
  print $ productOfFilteredSum input