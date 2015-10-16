-- HackerRank challenge - Algorithms - 2-2: sherlock and the beast

import Data.List (intercalate)

main :: IO ()
main = getLine >> getContents >>= putStrLn . intercalate "\n" . solve . map read . words

solve :: [Int] -> [String]
solve [] = []
solve (x:xs) = (if k == (-1, -1) then (show (-1)) else (replicate (fst k) '5' ++ replicate (snd k) '3')) : solve xs where k = cnt x (3 * (x `div` 3))

cnt :: Int -> Int -> (Int, Int)
cnt n k
    | k < 0 = (-1, -1)
    | (n-k) `mod` 5 == 0 = (k, n-k)
    | otherwise = cnt n (k-3)
