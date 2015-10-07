-- hackrank challenge - algorithm - 2-3: utopian tree

import Data.List (intercalate)
import Debug.Trace

main :: IO ()
main = getLine >> getContents >>= putStrLn . intercalate "\n" . map show . solve . map read . words

solve :: [Int] -> [Int]
solve = map (calc 1 1) where
    calc h k n
        | k > n = h
        | k `mod` 2 == 1 = calc (h*2) (k+1) n
        | otherwise = calc (h+1) (k+1) n 
