-- Codeforces 122A

import Data.List

main :: IO ()
main = getLine >>= putStrLn . solve . read where
    solve n = case find (\x -> n `mod` x == 0) $ map read $ lucky 3 ["4", "7"] of {
        Nothing -> "NO";
        Just _ -> "YES";
    } where
        lucky 1 xs = xs
        lucky n xs = k ++ [x++y | x <- xs, y <- k] where k = lucky (n-1) xs
