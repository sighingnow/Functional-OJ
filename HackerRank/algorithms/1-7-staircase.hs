-- Hankrank challenge - algorithm - 1-7: staircase

import Data.List

main :: IO ()
main = getLine >>= putStrLn . intercalate "\n" . solve . read

solve :: Int -> [String]
solve n = [replicate (n-i) ' ' ++ replicate i '#' | i <- [1..n]]