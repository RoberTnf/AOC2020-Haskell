module AOC2a where

import AOC
import Text.Regex.TDFA

-- import Text.Regex.TDFA

-- |
-- >>> isValid (1,3,'a',"abcde")
-- True
--
-- >> isValid (1,3,'b',"cdefg")
-- False
--
-- >> isValid (2,9,'c',"ccccccccc")
-- True
isValid :: (Int, Int, Char, String) -> Bool
isValid (min_, max_, char, pwd) =
  let count = length $ filter (== char) pwd
   in (count >= min_) && (count <= max_)

-- |
-- >>> format "1-3 a: abcde"
-- (1,3,'a',"abcde")
--
-- >>> format "1-3 b: cdefg"
-- (1,3,'b',"cdefg")
--
-- >>> format "2-9 c: ccccccccc"
-- (2,9,'c',"ccccccccc")
format :: String -> (Int, Int, Char, String)
format = intoTuple . head . getRegexGroups

intoTuple :: [String] -> (Int, Int, Char, String)
intoTuple [_, min_, max_, [char], pwd] = (read min_, read max_, char, pwd)
intoTuple x = error "Error in input format"

getRegexGroups :: String -> [[String]]
getRegexGroups x = x =~ "([0-9]+)-([0-9]+) ([a-zA-Z]): ([a-zA-Z]+)"

aoc2a :: IO ()
aoc2a = do
  input <- getInput 2
  let s2 = map format input
  print $ length . filter isValid $ s2