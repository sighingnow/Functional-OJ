-- Codeforces 110A

import           Data.List

main :: IO ()
main = getLine >>= putStrLn . solve where
    solve xs = case find (== (length $ filter (\c -> (c == '4') || (c == '7')) xs)) $ map read $ lucky 2 ["4", "7"] of {
        Nothing -> "NO";
        Just _ -> "YES";
    } where
        lucky 1 xs = xs
        lucky n xs = k ++ [x++y | x <- xs, y <- k] where k = lucky (n-1) xs
