module AOC2b where

import AOC
import Text.Regex.TDFA

type Row = (Int, Int, Char, String)

-- |
-- >>> isValid (1,3,'a',"abcde")
-- True
--
-- >> isValid (1,3,'b',"cdefg")
-- False
--
-- >> isValid (2,9,'c',"ccccccccc")
-- False
isValid' :: Row -> Maybe Bool
isValid' (firstIdx, secondIdx, char, pwd) =
  let getIndex l n = if length l >= n then Just (l !! n) else Nothing
      firstLetter = pwd `getIndex` (firstIdx - 1)
      secondLetter = pwd `getIndex` (secondIdx - 1)
   in do
        firstBool <- fmap (== char) firstLetter
        secondBool <- fmap (== char) secondLetter
        return (firstBool `xor` secondBool)

isValid :: Row -> Bool
isValid row =
  let unpack (Just x) = x
      unpack Nothing = False
   in unpack $ isValid' row

-- |
-- >>> format "1-3 a: abcde"
-- (1,3,'a',"abcde")
--
-- >>> format "1-3 b: cdefg"
-- (1,3,'b',"cdefg")
--
-- >>> format "2-9 c: ccccccccc"
-- (2,9,'c',"ccccccccc")
format :: String -> Row
format = intoRow . head . getRegexGroups

intoRow :: [String] -> Row
intoRow [_, min_, max_, [char], pwd] = (read min_, read max_, char, pwd)
intoRow x = error "Error in input format"

getRegexGroups :: String -> [[String]]
getRegexGroups x = x =~ "([0-9]+)-([0-9]+) ([a-zA-Z]): ([a-zA-Z]+)"

aoc2b :: IO ()
aoc2b = do
  input <- getInput 2
  let s2 = map format input
  print $ length . filter isValid $ s2