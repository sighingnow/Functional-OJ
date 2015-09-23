-- Codeforces 549A

import Data.List (sort)

main :: IO ()
main = do
    [n, m] <- fmap (map read . words) getLine
    getContents >>= print . solve [(i, j) | i <- [0..n-2], j <- [0..m-2]] . lines -- enumerate all positions.

solve :: [(Int, Int)] -> [String] -> Int
solve [] _ = 0
solve ((i,j):xs) mat = check + (solve xs mat) where check = if sort [mat!!i!!j, mat!!i!!(j+1), mat!!(i+1)!!j, mat!!(i+1)!!(j+1)] == "acef" then 1 else 0

