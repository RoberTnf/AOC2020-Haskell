module AOC11b where

import AOC
import Data.Bifunctor
import Data.List

-- $setup
-- >>> let startLayout = lines "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL\n"
-- >>> let stepOneLayout = lines "#.##.##.##\n#######.##\n#.#.#..#..\n####.##.##\n#.##.##.##\n#.#####.##\n..#.#.....\n##########\n#.######.#\n#.#####.##\n"
-- >>> let stepTwoLayout = lines "#.LL.LL.L#\n#LLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLL#\n#.LLLLLL.L\n#.LLLLL.L#\n"
-- >>> let stepThreeLayout = lines "#.L#.##.L#\n#L#####.LL\nL.#.#..#..\n##L#.##.##\n#.##.#L.##\n#.#####.#L\n..#.#.....\nLLL####LL#\n#.L#####.L\n#.L####.L#\n"

type Layout = [String]

type Position = (Int, Int)

type Direction = (Int, Int)

-- |
-- >>> solve startLayout
-- 26
solve :: Layout -> Int
solve = length . filter (== '#') . intercalate "" . fst . head . filter (uncurry (==)) . rollingWindow2 . iterate step

-- >>> step stepTwoLayout
-- ["#.LL.LL.L#","#LLLLLL.LL","L.L.L..L..","L#LL.LL.#L","L.#L.LL.#L","L.#LLL#.#L","..L.#.....","LLLLLLLLL#","#.LLLLLL.L","#.LLLLL.L#"]
--
-- >>> stepThreeLayout
-- ["#.L#.##.L#","#L#####.LL","L.#.#..#..","##L#.##.##","#.##.#L.##","#.#####.#L","..#.#.....","LLL####LL#","#.L#####.L","#.L####.L#"]

-- |
-- >>> step startLayout == stepOneLayout
-- True
-- >>> step stepOneLayout == stepTwoLayout
-- True
-- >>> step stepTwoLayout == stepThreeLayout
-- True
step layout =
  let neighbors = mapWithIndex (\(l, i) -> mapWithIndex (\(el, j) -> (el, countNeighbors layout (i, j))) l) layout
      step ('L', x) = if x == 0 then '#' else 'L'
      step ('#', x) = if x >= 5 then 'L' else '#'
      step (c, _) = c
   in map (map step) neighbors

mapWithIndex :: ((a, Int) -> c) -> [a] -> [c]
mapWithIndex f l = zipWith (curry f) l [0 .. length l]

-- |
-- >>> countNeighbors stepTwoLayout(0, 0)
-- 1
-- >>> countNeighbors stepOneLayout(0, 5)
-- 5
-- >>> countNeighbors stepTwoLayout(0, 4)
-- 0
--
-- >>> countNeighbors startLayout (0, 0)
-- 0
countNeighbors :: Layout -> Position -> Int
countNeighbors layout (x, y) =
  let directions = filter (/= (0, 0)) $ cartesianProduct [-1 .. 1] [-1 .. 1]
      maxDim = max xLength yLength
      offsets = map (\(x, y) -> map (\a -> (x * a, y * a)) [1 .. maxDim]) directions
      xLength = length layout
      yLength = length (head layout)
      positions = map (\offset -> [(x + ox, y + oy) | (ox, oy) <- offset, xLength > x + ox && yLength > y + oy && x + ox >= 0 && y + oy >= 0]) offsets
   in length . filter (== '#') . map head . filter (not . null) . map (filter (/= '.') . map (getSeat layout)) $ positions

getSeat :: Layout -> Position -> Char
getSeat layout (x, y) = layout !! x !! y

aoc11b :: IO ()
aoc11b = do
  input <- getInput 11
  print . solve $ input
