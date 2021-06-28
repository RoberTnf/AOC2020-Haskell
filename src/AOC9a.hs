-- Things got a little more difficult here. Lots of small issues with regexp, that I added into the doctests.
-- Also had some issues dealing with Lookup and its Maybe Bool type, solved it by using the justAndTrue function.

module AOC9a where

import AOC
import qualified Data.Dequeue as D

-- |
-- >>> let s = lines "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576\n\n"
-- >>> solve s 5
-- 127
findBug :: [Int] -> Int -> Dequeue Int -> Int
findBug (x : xs) n dq
  | length dq < n = findBug xs n (pushFrontLimited n dq x)
  | otherwise = if not $ any (\y -> (x - y) `elem` dq) dq then x else findBug xs n (pushFrontLimited n dq x)
findBug _ _ _ = error "No bug found"

solve :: [String] -> Int -> Int
solve input n =
  let s = map (read :: String -> Int) input
   in findBug s n (D.fromList [])

aoc9a :: IO ()
aoc9a = do
  input <- getInput 9
  print . solve input $ 25
