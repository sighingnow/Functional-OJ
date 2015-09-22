-- Codeforces 489B

import Data.List

main :: IO ()
main = do
    n <- fmap read getLine
    ns <- fmap (map read . words) getLine
    m <- fmap read getLine
    ms <- fmap (map read . words) getLine
    print $ solve n m (sort ns) (sort ms)

solve :: Int -> Int -> [Int] -> [Int] -> Int
solve n m ns ms = snd $ foldl (\(mss, acc) b -> case (findIndex (\a -> (-1 <= b-a) && (1 >= b-a)) mss)
    of {
        Nothing -> (mss, acc);
        Just k -> (drop (k+1) mss, acc+1) ;
    }) (ms, 0) ns
