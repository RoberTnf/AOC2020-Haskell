-- This is solved much better in exercise 12b
module AOC12a where

import AOC
import Data.Bifunctor

-- $setup
-- >>> input = lines "F10\nN3\nF7\nR90\nF11\n"

type Point2D = (Int, Int)

-- |
-- >>> rotate (-1, 0) input
-- [(-1,0),(-1,0),(-1,0),(0,1),(0,1)]
rotate :: Point2D -> [String] -> [Point2D]
rotate facing (command : commands) =
  let number = read . tail $ command :: Int
      currentFacing = case head command of
        'R' -> iterate rotateRight facing !! (number `mod` 360 `div` 90)
        'L' -> iterate rotateRight facing !! (4 - (number `mod` 360 `div` 90))
        _ -> facing
   in currentFacing : rotate currentFacing commands
rotate _ [] = []

move :: Point2D -> String -> Point2D
move facing command =
  let number = read . tail $ command :: Int
   in case head command of
        'N' -> (0, number)
        'S' -> (0, - number)
        'E' -> (number, 0)
        'W' -> (- number, 0)
        'F' -> bimap (* number) (* number) facing
        _ -> (0, 0)

rotateRight :: Point2D -> Point2D
rotateRight (x, y) = (y, - x)

-- |
-- >>> solve input
-- 25
solve commands =
  let facings = rotate (1, 0) commands
      finalPos = foldr (\(a, b) (x, y) -> (a + x, b + y)) (0, 0) $ zipWith move facings commands
      manhattanDist = (abs . fst $ finalPos) + (abs . snd $ finalPos)
   in manhattanDist

aoc12a :: IO ()
aoc12a = do
  input <- getInput 12
  print . solve $ input
