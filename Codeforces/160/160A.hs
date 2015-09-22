-- Codeforces 160A

import Data.List

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    getLine >>= print . solve . sortBy (\a b -> compare b a) . map read . words

solve :: [Int] -> Int
solve xs = length $ takeWhile (\x -> x <= (sum xs) `div` 2) $ foldl (\x a -> x ++ [last x + a]) [0] xs
