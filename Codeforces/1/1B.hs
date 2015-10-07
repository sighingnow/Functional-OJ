-- Codeforces 1B

import Data.List
import Data.Char

main :: IO ()
main = interact $ unlines . map solve . tail . lines

-- use groupBy to detect the kind of input.

solve :: String -> String
solve s = case groupBy (\a b -> isDigit a == isDigit b) s
    of {
        [_, r, _, c]    -> (reverse $ i2s $ read c) ++ r;
        [c, r]          -> "R" ++ r ++ "C" ++ (show $ s2i c)
    }


s2i :: String -> Int
s2i = foldl' (\s d -> 26 * s + ord d - ord 'A' + 1) 0

i2s :: Int -> String
i2s 0 = []
i2s a = chr (ord 'A' + (a-1) `mod` 26) : (i2s $ (a-1) `div` 26)

