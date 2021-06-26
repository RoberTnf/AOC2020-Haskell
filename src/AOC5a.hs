-- Things got a little more difficult here. Lots of small issues with regexp, that I added into the doctests.
-- Also had some issues dealing with Lookup and its Maybe Bool type, solved it by using the justAndTrue function.

module AOC5a where

import AOC

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

aoc5a :: IO ()
aoc5a = do
  input <- getInput 5
  print . foldr (max . getIdx . separateInput) 0 $ input
