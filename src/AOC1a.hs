module AOC1a where

import AOC
import Data.Bifunctor (Bifunctor (bimap))

-- |
-- >>> productOfFilteredSum ["1721", "979", "366", "299", "675", "1456"]
-- 514579
productOfFilteredSum :: [String] -> Int
productOfFilteredSum input =
  let combinations = map (Data.Bifunctor.bimap read read) $ cartesianProduct input input :: [(Int, Int)]
      validCombination = filter (\(a, b) -> a + b == 2020) combinations
   in uncurry (*) $ head validCombination

aoc1a :: IO ()
aoc1a = do
  input <- getInput 1
  print $ productOfFilteredSum input
