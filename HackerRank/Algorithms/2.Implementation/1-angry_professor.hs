-- HackerRank challenge - Algorithms - 2-1: angry professor

import Control.Monad
import Data.List (intercalate)

main :: IO ()
main = getLine >> getContents >>= putStrLn . intercalate "\n" . solve . map (map read . words) . lines

solve :: [[Int]] -> [String]
solve [] = []
solve ([n,k]:ns:xs) = (if (length $ filter (<= 0) ns) < k then "YES" else "NO") : solve xs
