-- hackrank challenge - algorithm - 2-8: chocolate feast

import Data.List

main :: IO ()
main = do
    getLine
    getContents >>= putStrLn . intercalate "\n" . map show . solve . map (map read . words) . lines

solve :: [[Int]] -> [Int]
solve = map (\[n,c,m] -> let x = n `div` c in x + iter m x) where
    iter m k
        | k < m = 0
        | otherwise = (k `div` m) + (iter m ((k `div` m) + (k `mod` m)))
