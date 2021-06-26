-- Things got a little more difficult here. Lots of small issues with regexp, that I added into the doctests.
-- Also had some issues dealing with Lookup and its Maybe Bool type, solved it by using the justAndTrue function.

module AOC5b where

import AOC
import Data.List

type BoardPass = (String, String)

-- |
-- >>> getRow "FBFBBFF"
-- 44
--
-- >>> getRow "BFFFBBF"
-- 70
--
-- >>> getRow "FFFBBBF"
-- 14
--
-- >>> getRow "BBFFBBF"
-- 102
getRow :: String -> Int
getRow (c : xs) =
  let p = if c == 'B' then 1 else 0
   in p * 2 ^ length xs + getRow xs
getRow [] = 0

-- |
-- >>> getCol "RLR"
-- 5
--
-- >>> getCol "RRR"
-- 7
--
-- >>> getCol "RRR"
-- 7
--
-- >>> getCol "RLL"
-- 4
getCol :: String -> Int
getCol (c : xs) =
  let p = if c == 'R' then 1 else 0
   in p * 2 ^ length xs + getCol xs
getCol [] = 0

-- |
-- >>> getIdx . separateInput $  "FBFBBFFRLR"
-- 357
--
-- >>> getIdx . separateInput $  "BFFFBBFRRR"
-- 567
--
-- >>> getIdx . separateInput $  "FFFBBBFRRR"
-- 119
--
-- >>> getIdx . separateInput $  "BBFFBBFRLL"
-- 820
getIdx :: BoardPass -> Int
getIdx (rowStr, colStr) = getRow rowStr * 8 + getCol colStr

separateInput :: String -> BoardPass
separateInput = splitAt 7

aoc5b :: IO ()
aoc5b = do
  input <- getInput 5
  let presentTickets = map (getIdx . separateInput) input
  let possibleTickets = [1 .. (foldr max 0 presentTickets)]
  print . head $ filter (\x -> x+1 `elem` presentTickets && x-1 `elem` presentTickets) $ possibleTickets \\ presentTickets
