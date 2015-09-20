-- Codeforces 96A

import Data.List

main :: IO ()
main = getLine >>= putStrLn . solve where
    solve xs = if (count xs) then "YES" else "NO" where
        count [] = False
        count (x:xs) = if (length (takeWhile (== x) xs) >= 6) then True else count (dropWhile (== x) xs) where 

