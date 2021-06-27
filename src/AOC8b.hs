-- Things got a little more difficult here. Lots of small issues with regexp, that I added into the doctests.
-- Also had some issues dealing with Lookup and its Maybe Bool type, solved it by using the justAndTrue function.

module AOC8b where

import AOC
import Data.Maybe

type Program = [String]

-- $setup
-- >>> let program = lines "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6\n"

-- |
-- >>> solve program
-- Nothing
execute :: Program -> Int -> Int -> [Int] -> Maybe Int
execute program line acc visited
  | line `elem` visited = Nothing
  | line == length program = Just acc
  | otherwise =
    let (operator, n) = splitAt 3 $ program !! line
        (sign, n') = splitAt 2 n
        num = if sign == " +" then read n' else - read n'
     in case operator of
          "acc" -> execute program (line + 1) (acc + num) (line : visited)
          "nop" -> execute program (line + 1) acc (line : visited)
          "jmp" -> execute program (line + num) acc (line : visited)
          s -> error $ "Wrong instruction: " ++ s

-- |
-- >>> allPrograms program
-- [Just ["nop +0","acc +1","jmp +4","acc +3","jmp -3","acc -99","acc +1","jmp -4","acc +6"],Just ["jmp +0","acc +1","jmp +4","acc +3","jmp -3","acc -99","acc +1","jmp -4","acc +6"],Nothing,Just ["nop +0","acc +1","nop +4","acc +3","jmp -3","acc -99","acc +1","jmp -4","acc +6"],Nothing,Just ["nop +0","acc +1","jmp +4","acc +3","nop -3","acc -99","acc +1","jmp -4","acc +6"],Nothing,Nothing,Just ["nop +0","acc +1","jmp +4","acc +3","jmp -3","acc -99","acc +1","nop -4","acc +6"],Nothing]
allPrograms :: Program -> [Maybe Program]
allPrograms program = Just program : map (alternativeProgram program) [0 .. length program -1]

-- |
-- >>> alternativeProgram program 0
-- Just ["jmp +0","acc +1","jmp +4","acc +3","jmp -3","acc -99","acc +1","jmp -4","acc +6"]
alternativeProgram :: Program -> Int -> Maybe Program
alternativeProgram program i =
  let line = program !! i
      (topProgram, bottomProgram') = splitAt i program
      bottomProgram = tail bottomProgram'
   in do
        altLine <- alternativeLine line
        return $ topProgram ++ [altLine] ++ bottomProgram

alternativeLine :: String -> Maybe String
alternativeLine line = case head line of
  'a' -> Nothing
  'n' -> Just ("jmp" ++ drop 3 line)
  'j' -> Just ("nop" ++ drop 3 line)
  _ -> error "Wrong instruction"

solve :: Program -> Maybe Int
solve p = execute p 0 0 []

-- |
-- >>> fix program
-- 8
fix p = unwrap . head . filter isJust $ map (>>= solve) $ allPrograms p

aoc8b :: IO ()
aoc8b = do
  input <- getInput 8
  print . fix $ input
