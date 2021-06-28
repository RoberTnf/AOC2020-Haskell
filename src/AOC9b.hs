module AOC9b where

import AOC
import qualified Data.Dequeue as D
import Data.Maybe

findBug :: [Int] -> Int -> Dequeue Int -> Int
findBug (x : xs) n dq
  | length dq < n = findBug xs n (pushFrontLimited n dq x)
  | otherwise = if not $ any (\y -> (x - y) `elem` dq) dq then x else findBug xs n (pushFrontLimited n dq x)
findBug _ _ _ = error "No bug found"

-- |
-- >>> let s = lines "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576\n\n"
-- >>> solve s 5
-- 62
solve :: [String] -> Int -> Int
solve input n =
  let s = map (read :: String -> Int) input
      bug = findBug s n (D.fromList [])
   in findContiguousSet bug s

findContiguousSet :: Int -> [Int] -> Int
findContiguousSet bug (x : xs)
  | isJust c = maximum (unwrap c) + minimum (unwrap c)
  | otherwise = findContiguousSet bug xs
  where
    c = takeUntil (\x -> sum x `compare` bug) (x : xs) []
findContiguousSet _ [] = error "Exhausted input"

-- |
-- >>> takeUntil (\x -> sum x `compare` 100) [50,45,5,70] []
-- Just [5,45,50]
takeUntil :: ([Int] -> Ordering) -> [Int] -> [Int] -> Maybe [Int]
takeUntil f (x : xs) s
  | f ss == GT = Nothing
  | f ss == LT = takeUntil f xs ss
  | otherwise = if length ss > 1 then Just ss else takeUntil f xs ss
  where
    ss = x : s
takeUntil _ [] _ = error "Exhausted input"

aoc9b :: IO ()
aoc9b = do
  input <- getInput 9
  print . solve input $ 25
