-- HackerRank challenge - Algorithms - 2-5: sherlock and squares

import           Data.List

main :: IO ()
main = getLine >> getContents >>= putStrLn . intercalate "\n" . map show . solve . map (map read . words) . lines

squares :: [Int]
squares = map (\x -> x*x) [1..32000]

solve :: [[Int]] -> [Int]
solve = map (\[l,r] -> length $ filter (\k -> k >= l && k <= r) squares)

