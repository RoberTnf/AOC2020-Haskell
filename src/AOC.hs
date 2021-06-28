module AOC where

import qualified Data.Dequeue as D

type Dequeue = D.BankersDequeue

getInput :: Int -> IO [String]
getInput day = fmap lines . readFile $ "input/" ++ show day ++ ".input"

cartesianProduct :: Monad m => m a -> m b -> m (a, b)
cartesianProduct xs ys = xs >>= \x -> ys >>= \y -> return (x, y)

-- https://stackoverflow.com/a/36656030
xor :: Eq a => a -> a -> Bool
xor a b = a /= b

unwrap :: Maybe p -> p
unwrap (Just x) = x
unwrap Nothing = error "Unwrap nothing"

-- |
-- >>> let d = D.fromList [1,2,3] :: D.BankersDequeue Int
-- >>> pushFrontLimited 3 d 4
-- Dequeue [4,1,2]
-- >>> pushFrontLimited 4 d 4
-- Dequeue [4,1,2,3]
pushFrontLimited :: Int -> Dequeue a -> a -> Dequeue a
pushFrontLimited l dq el
  | length dq < l = D.pushFront dq el
  | otherwise = D.pushFront (snd . unwrap $ D.popBack dq) el