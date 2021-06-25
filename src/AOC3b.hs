module AOC3b where

import AOC
import Text.Regex.TDFA

type Step = (Int, Int)

-- $setup
-- >>> let input = lines "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"

-- |
-- >>> countTreesInPath input (3, 1)
-- 7
countTreesInPath :: [String] -> Step -> Int
countTreesInPath forest step =
  let len = length . head $ forest
   in solve forest 0 len step

solve :: [String] -> Int -> Int -> Step -> Int
solve (row : forest) i len (horizontalStep, verticalStep) =
  let i' = ((i + horizontalStep) `mod` len)
      newForest = if length forest >= verticalStep then iterate tail forest !! (verticalStep - 1) else []
      isTree = if row !! i == '#' then 1 else 0
   in isTree + solve newForest i' len (horizontalStep, verticalStep)
solve [] _ _ _ = 0

defaultSteps :: [(Int, Int)]
defaultSteps = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

-- |
-- >>> product . map (countTreesInPath input) $ defaultSteps
-- 336
aoc3b :: IO ()
aoc3b = do
  input <- getInput 3
  print . product . map (countTreesInPath input) $ defaultSteps