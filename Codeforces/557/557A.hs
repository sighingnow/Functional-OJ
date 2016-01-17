-- Codeforces 557A

import           Data.List (intercalate)

main :: IO ()
main = getContents >>= putStrLn . intercalate " " . map show . solve . map read . words

solve :: [Int] -> [Int]
solve [n, min1, max1, min2, max2, min3, max3]
    | max1+min2+min3 >= n = [n-min2-min3, min2, min3]
    | max1+max2+min3 >= n = [max1, n-max1-min3, min3]
    | otherwise = [max1, max2, n-max1-max2]
