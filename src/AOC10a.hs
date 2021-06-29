module AOC10a where

import AOC
import Data.List

-- $setup
-- >>> let short = format . lines $ "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4\n"
-- >>> let long = format . lines $ "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3\n"

-- |
-- >>> solve short
-- 35
--
-- >>> solve long
-- 220
--
solve :: [Int] -> Int
solve input' =
    let input = (maximum input' +3):0:input'
        differences = map (\(x,y) -> y - x) $ rollingWindow2 . sort $ input
        count n = (length . filter (==n) $ differences) 
    in count 3 * count 1 



-- |
-- >>> short
-- [16,10,15,5,1,11,7,19,6,12,4]
format :: [String] -> [Int]
format = map read


aoc10a :: IO ()
aoc10a = do
  input <- getInput 10
  print . solve .format $  input
