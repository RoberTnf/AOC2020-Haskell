module AOC12b where

import AOC
import Data.Bifunctor

-- $setup
-- >>> input = lines "F10\nN3\nF7\nR90\nF11\n"

type Point2D = (Int, Int)

-- |
-- >>> doOrder (0, 0) (10, 1) input
-- [(100,10),(100,10),(170,38),(170,38),(214,-72)]
doOrder :: Point2D -> Point2D -> [String] -> [Point2D]
doOrder position waypoint (command : commands) =
  let number = read . tail $ command :: Int
      (x, y) = position
      (wx, wy) = waypoint
      currentWaypoint = case head command of
        'R' -> iterate rotateRight waypoint !! (number `mod` 360 `div` 90)
        'L' -> iterate rotateRight waypoint !! (4 - (number `mod` 360 `div` 90))
        'E' -> (wx + number, wy)
        'W' -> (wx - number, wy)
        'N' -> (wx, wy + number)
        'S' -> (wx, wy - number)
        _ -> waypoint
      currentPosition = case head command of
        'F' -> (x + number * wx, y + number * wy)
        _ -> position
   in currentPosition : doOrder currentPosition currentWaypoint commands
doOrder _ _ [] = []

-- |
-- >>> map rotateRight [(10, 1), (10, -1), (-10, -1), (-10, 1)]
-- [(1,-10),(-1,-10),(-1,10),(1,10)]
rotateRight :: Point2D -> Point2D
rotateRight (x, y) = (y, - x)

-- |
-- >>> solve input
-- 286
solve commands =
  let finalPos = last $ doOrder (0,0) (10, 1) commands
      manhattanDist = (abs . fst $ finalPos) + (abs . snd $ finalPos)
   in manhattanDist

aoc12b :: IO ()
aoc12b = do
  input <- getInput 12
  print . solve $ input
