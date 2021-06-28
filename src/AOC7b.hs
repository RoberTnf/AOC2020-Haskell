module AOC7b where

import AOC
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Text.Regex.TDFA

-- $setup
-- >>> let s = "dull aqua bags contain 4 dark fuchsia bags, 1 shiny purple bag."
-- >>> let bigS = lines "light red bags contain 2 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.\n"
-- >>> let bigS2 = lines "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 vibrant plum bags.\nvibrant plum bags contain no other bags"

type BagMap = Map.Map String (Map.Map String Int)

inputToMap :: [String] -> BagMap
inputToMap input = Map.fromList $ map (\s -> (getMainBag s, getSecondaryBags s)) input

-- |
-- >>> bagsCanCarry (inputToMap bigS2) "shiny gold"
-- 6
--
-- >>> bagsCanCarry (inputToMap bigS) "shiny gold"
-- 32
bagsCanCarry :: BagMap -> String -> Int
bagsCanCarry input s =
  let childBags = unwrap $ Map.lookup s input
   in case length childBags of
        0 -> 0
        _ -> sum $ Map.mapWithKey (\name n -> n * (bagsCanCarry input name + 1)) childBags

-- |
-- >>> getMainBag s
-- "dull aqua"
getMainBag :: String -> String
getMainBag s = s =~ "[a-z]+ [a-z]+"

-- |
-- >>> getSecondaryBags s
-- fromList [("dark fuchsia",4),("shiny purple",1)]
getSecondaryBags :: String -> Map.Map String Int
getSecondaryBags s =
  let matches = getAllTextMatches (s =~ "[0-9]+ [a-z]+ [a-z]+")
   in Map.fromList $ map (toTuple . tail . head . splitIntoComponents) matches

toTuple :: [String] -> (String, Int)
toTuple [v, k] = (k, read v)
toTuple _ = error "Wrong input"

splitIntoComponents :: String -> [[String]]
splitIntoComponents x = x =~ "([0-9]+) (.*)"

aoc7b :: IO ()
aoc7b = do
  input <- getInput 7
  print $ bagsCanCarry (inputToMap input) "shiny gold"
