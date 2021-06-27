module AOC where

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
