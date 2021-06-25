module AOC3a where

import AOC
import Text.Regex.TDFA

-- |
-- >>> countTreesInPath $ lines "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"
-- 7
countTreesInPath :: Num p1 => [String] -> p1
countTreesInPath forest =
  let len = length . head $ forest
   in solve forest 0 len

solve :: Num p => [String] -> Int -> Int -> p
solve (row : forest) i len =
  let i' = ((i + 3) `mod` len)
   in if row !! i == '#'
        then 1 + solve forest i' len
        else solve forest i' len
solve [] _ _ = 0

aoc3a :: IO ()
aoc3a = do
  input <- getInput 3
  print $ countTreesInPath input