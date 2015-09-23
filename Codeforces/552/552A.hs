-- Codeforces 552A

import qualified Data.Map.Strict as Map

-- use Data.Map to storage the matrix

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    getContents >>= print . sum . Map.elems . solve Map.empty . map read . words

solve :: Map.Map (Int, Int) Int -> [Int] -> Map.Map (Int, Int) Int
solve m [] = m
solve m (x1:y1:x2:y2:xs) = solve newm xs where
    newm = foldl (\mm loc -> Map.insertWith (+) loc 1 mm) m [(i, j) | i <- [x1..x2], j <- [y1..y2]]
