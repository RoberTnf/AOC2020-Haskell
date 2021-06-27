-- Things got a little more difficult here. Lots of small issues with regexp, that I added into the doctests.
-- Also had some issues dealing with Lookup and its Maybe Bool type, solved it by using the justAndTrue function.

module AOC8a where

import AOC

type Program = [String]

-- |
-- >>> solve $ lines "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6\n"
-- 5
execute :: Program -> Int -> Int -> [Int] -> Int
execute program line acc visited
  | line `elem` visited = acc
  | otherwise = let
      (operator, n) = splitAt 3 $ program !! line
      (sign, n') = splitAt 2 n
      num = if sign == " +" then read n' else - read n'
    in case operator of
      "acc" -> execute program (line + 1) (acc + num) (line:visited)
      "nop" -> execute program (line + 1) acc (line:visited)
      "jmp" -> execute program (line + num) acc (line:visited)
      s -> error $ "Wrong instruction: " ++ s

solve :: Program -> Int
solve p = execute p 0 0 []

aoc8a :: IO ()
aoc8a = do
  input <- getInput 8
  print . solve $ input 
