-- Codeforces 546B

import Data.List

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    getLine >>= print . solve . sort . map read . words

-- note: only cdn increase the coolness value, can't decrease it, so need do sort.

solve :: [Int] -> Int
solve [] = 0
solve [_] = 0
solve (x:y:xs) = if (y <= x) then (x-y+1) + solve (x+1:xs) else solve (y:xs)

