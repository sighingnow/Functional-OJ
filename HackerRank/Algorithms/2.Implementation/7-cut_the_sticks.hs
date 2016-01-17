-- HackerRank challenge - Algorithms - 2-7: cut the sticks

import           Data.List

main :: IO ()
main = do
    getLine
    getContents >>= putStrLn . intercalate "\n" . map show . solve . map read . words

solve :: [Int] -> [Int]
solve [] = []
solve xs = (length xs) : solve (filter (> minimum xs) xs)
