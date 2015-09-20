-- Codeforces 339A

import Data.List

main :: IO ()
main = getLine >>= putStrLn . solve . zip [1..]

solve :: [(Int, Char)] -> String
solve xs = tail $ foldl (\a b -> (a ++ [snd (cs !! ((fst b) `div` 2))] ++ [snd b])) [] ns where
    ns = zip [1..] $ map snd $ sortBy (\a b -> (compare (snd a) (snd b))) $ filter (\x -> (fst x) `mod` 2 == 1) xs
    cs = [(0, ' ')] ++ filter (\x -> (fst x) `mod` 2 == 0) xs