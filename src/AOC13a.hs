module AOC13a where

import AOC (getInput)
import Data.Char (isNumber)
import Data.List.Split (splitOn)
import GHC.Real (infinity)

-- $setup
-- >>> input = lines "939\n7,13,x,x,59,x,31,19"

-- |
-- >>> format input
-- (939,[7,13,59,31,19])
format :: [String] -> (Int, [Int])
format [timestamp, ids] =
  let parse_bus_ids ids = map read . filter (/= "x") $ splitOn "," ids
   in (read timestamp, parse_bus_ids ids)
format _ = error "Malformed input"

-- |
-- >>> solve . format $ input
-- 295
solve :: (Int, [Int]) -> Int
solve (timestamp, ids) =
  let list_ids = zip (map (\m -> m - mod timestamp m) ids) ids
      (to_wait, bus_id) = foldr (\(min, id) (min2, id2) -> if min < min2 then (min, id) else (min2, id2)) (head list_ids) list_ids
   in to_wait * bus_id

aoc13a :: IO ()
aoc13a = do
  input <- getInput 13
  print . solve . format $ input
