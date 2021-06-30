{-
First program that takes more than 0.1 seconds to run. It takes 6.6s but it
is kind of expected due to the cellular automaton we have to evolve.
-}
module AOC11a where

import AOC
import Data.Bifunctor
import Data.List

-- $setup
-- >>> let startLayout = lines "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL\n"
-- >>> let stepOneLayout = lines "#.##.##.##\n#######.##\n#.#.#..#..\n####.##.##\n#.##.##.##\n#.#####.##\n..#.#.....\n##########\n#.######.#\n#.#####.##\n"
-- >>> let stepTwoLayout = lines "#.LL.L#.##\n#LLLLLL.L#\nL.L.L..L..\n#LLL.LL.L#\n#.LL.LL.LL\n#.LLLL#.##\n..L.L.....\n#LLLLLLLL#\n#.LLLLLL.L\n#.#LLLL.##\n"

type Layout = [String]

type Position = (Int, Int)

-- |
-- >>> solve startLayout
-- 37
solve :: Layout -> Int
solve = length . filter (== '#') . intercalate "" . fst . head . filter (uncurry (==)) . rollingWindow2 . iterate step

-- |
-- >>> step startLayout == stepOneLayout
-- True
-- >>> step stepOneLayout == stepTwoLayout
-- True
step :: Layout -> Layout
step layout =
  let neighbors = mapWithIndex (\(l, i) -> mapWithIndex (\(el, j) -> (el, countNeighbors layout (i, j))) l) layout
      step ('L', x) = if x == 0 then '#' else 'L'
      step ('#', x) = if x >= 4 then 'L' else '#'
      step (c, _) = c
   in map (map step) neighbors

mapWithIndex :: ((a, Int) -> c) -> [a] -> [c]
mapWithIndex f l = zipWith (curry f) l [0 .. length l]

-- |
-- >>> countNeighbors stepTwoLayout(0, 0)
-- 1
-- >>> countNeighbors stepTwoLayout(0, 5)
-- 1
-- >>> countNeighbors stepTwoLayout(0, 4)
-- 0
countNeighbors :: Layout -> Position -> Int
countNeighbors layout (x, y) =
  let offsets = cartesianProduct [-1 .. 1] [-1 .. 1]
   in foldr ((+) . isOccupied layout . Data.Bifunctor.bimap (x +) (y +)) 0 offsets - isOccupied layout (x, y)

-- |
-- >>> isOccupied stepTwoLayout (0,0)
-- 1
-- >>> isOccupied stepTwoLayout (0,1)
-- 0
-- >>> isOccupied stepTwoLayout (length stepTwoLayout,0)
-- 0
-- >>> isOccupied stepTwoLayout (0,0)
-- 1
-- >>> isOccupied stepTwoLayout (-1,-1)
-- 0
isOccupied :: Layout -> Position -> Int
isOccupied layout (x, y) =
  let xLength = length layout
      yLength = length (head layout)
   in if xLength > x && yLength > y && x >= 0 && y >= 0
        then case layout !! x !! y of
          '#' -> 1
          _ -> 0
        else 0

aoc11a :: IO ()
aoc11a = do
  input <- getInput 11
  print . solve $ input
