-- HackerRank challenge - Algorithms - 2-10: cavity map

import           Data.Char
import           Data.List

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    getContents >>= putStrLn . intercalate "\n" . solve n . lines

solve :: Int -> [String] -> [String]
solve n mat = [[detect n mat i j | j <- [0..n-1]] | i <- [0..n-1]]

detect :: Int -> [String] -> Int -> Int -> Char
detect n mat i j
    | i == 0 || i == n-1 || j == 0 || j == n-1 = mat!!i!!j
    | otherwise = if and $ map (\(dx,dy) -> mat!!i!!j > mat!!(i+dx)!!(j+dy)) [(-1,0), (1,0), (0,-1), (0,1)] then 'X' else mat!!i!!j

