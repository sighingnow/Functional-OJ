-- HackerRank challenge - Algorithms - 2-6: service lane

import Data.List

main :: IO ()
main = do
    getLine
    ns <- fmap (map read . words) getLine :: IO [Int]
    getContents >>= putStrLn . intercalate "\n" . map show . solve ns . map (map read . words) . lines

solve :: [Int] -> [[Int]] -> [Int]
solve ns = map (\[i, j] -> minimum (drop i $ take (j+1) ns))
