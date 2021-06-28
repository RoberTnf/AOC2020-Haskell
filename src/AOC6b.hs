module AOC6b where

import AOC
import Data.List
import Data.List.Split
import qualified Data.Set as Set

-- |
-- >>> separateByGroup "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n"
-- ["abc","a\nb\nc","ab\nac","a\na\na\na","b\n"]
separateByGroup :: String -> [String]
separateByGroup = splitOn "\n\n"

separateByPerson :: String -> [String]
separateByPerson = splitOn "\n"

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = map head . group . sort

-- |
-- >>> commonInGroup ["ab", "ac"]
-- fromList "a"
--
-- >>> commonInGroup ["a", "b", "c"]
-- fromList ""
-- 
-- >>> commonInGroup ["a", "a", "a"]
-- fromList "a"
-- >>> commonInGroup ["abc"]
-- fromList "abc"
-- >>> commonInGroup ["b"]
-- fromList "b"
--
commonInGroup :: [String] -> Set.Set Char
commonInGroup = foldr (Set.intersection . Set.fromList) $ Set.fromList ['a' .. 'z']

-- |
-- >>> countYes "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
-- 6
countYes :: String -> Int
countYes = sum . map (length . commonInGroup . separateByPerson) . separateByGroup

aoc6b :: IO ()
aoc6b = do
  input <- getInput 6
  print . countYes . init . unlines $ input
