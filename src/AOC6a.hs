-- Things got a little more difficult here. Lots of small issues with regexp, that I added into the doctests.
-- Also had some issues dealing with Lookup and its Maybe Bool type, solved it by using the justAndTrue function.

module AOC6a where

import AOC
import Data.List
import Data.List.Split

-- |
-- >>> separateByGroup "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n"
-- ["abc","a\nb\nc","ab\nac","a\na\na\na","b\n"]
separateByGroup :: String -> [String]
separateByGroup = splitOn "\n\n"

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = map head . group . sort

-- |
-- >>> countYes "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n"
-- 11
countYes :: String -> Int
countYes = sum . map (length . removeDuplicates . filter (/= '\n')) . separateByGroup

aoc6a :: IO ()
aoc6a = do
  input <- getInput 6
  print . countYes . unlines $ input
