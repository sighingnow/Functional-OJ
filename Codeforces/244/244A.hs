-- Codeforces 244A

import           Data.List

main :: IO ()
main = getContents >>= putStrLn . solve . map read . words

solve :: [Int] -> String
solve (n:k:xs) = intercalate "\n" $ map (intercalate " " . map show . sort) $ transpose $ (xs:) $ map (\i -> take k $ drop ((i-1)*k) other) [1..n-1] where other = filter (`notElem` xs) [1..n*k]

