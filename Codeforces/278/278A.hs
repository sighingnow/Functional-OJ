-- Codeforces 278A

import Data.List

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    path <- fmap (map read . words) getLine :: IO [Int]
    aim <- fmap (map read . words) getLine :: IO [Int]
    print $ solve n (concat $ replicate 2 path) (sort aim) where
        solve n xs [s, t] = min (len s t) (len t (s+n)) where
            len a b = sum $ take (b-a) $ drop (a-1) xs
