-- Codeforces 472B

import Data.List

main :: IO ()
main = do
    [n, k] <- fmap (map read . words) getLine :: IO [Int]
    people <- fmap (reverse . sort . map read . words) getLine :: IO [Int]
    print $ solve k people where
        solve k people
            | length people == 0 = 0
            | length people <= k = 2 * (maximum people - 1)
            | otherwise = 2 * (head people - 1) + solve k (drop k people) 