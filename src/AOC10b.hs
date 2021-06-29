module AOC10b where

import AOC
import qualified AOC10a as A
import Data.List

-- $setup
-- >>> let short = A.format . lines $ "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4\n"
-- >>> let long = A.format . lines $ "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3\n"

-- |
-- >>> solve short
-- 8
--
-- >>> solve long
-- 19208
solve :: [Int] -> Int
solve input' =
  let input = (maximum input' + 3) : 0 : input' -- The input has two extra slots
      differences = map (\(x, y) -> y - x) $ rollingWindow2 . sort $ input -- List of differences in Jolts
   in foldr ((*) . getPossiblePaths . length) 1 $ filter (\x -> head x == 1) . group $ differences -- Only number of contiguous differences of 1 matter.

{-
Pretty proud of this one!

I realized quite quickly that the solution only depended on the number of
continuous ones.

However, it was somewhat hard to figure out the recursion relation getPossiblePaths,
I almost resorted to calculating it with brute force for any n, but I'm
glad I didn't, as the solution is quite efficient and elegant.

The key was to realize that after 3 (maximum adapter), you would go to
n-1, n-2 and n-3, depending on your first step.

Could be memoized, as it scales quite hard with n, but there's no need for this problem.
-}

-- |
-- >>> getPossiblePaths 4
-- 7
--
-- >>> getPossiblePaths 5
-- 13
getPossiblePaths :: Int -> Int
getPossiblePaths 1 = 1
getPossiblePaths 2 = 2
getPossiblePaths 3 = 4
getPossiblePaths n = getPossiblePaths (n -1) + getPossiblePaths (n - 2) + getPossiblePaths (n -3)

aoc10b :: IO ()
aoc10b = do
  input <- getInput 10
  print . solve . A.format $ input
