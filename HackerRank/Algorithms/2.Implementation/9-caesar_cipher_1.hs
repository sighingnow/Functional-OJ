-- HackerRank challenge - Algorithms - 2-9: caesar cipher 1

import           Data.Char
import           Data.List

main :: IO ()
main = do
    n <- readLn
    s <- getLine
    k <- readLn
    putStrLn $ solve n s k

solve :: Int -> String -> Int -> String
solve n s k = map convert s where
    convert c
        | 'a' <= c && 'z' >= c = chr (ord 'a' + ((ord c) + k - (ord 'a')) `mod` 26)
        | 'A' <= c && 'Z' >= c = chr (ord 'A' + ((ord c) + k - (ord 'A')) `mod` 26)
        | otherwise = c
