-- Codeforces 266A

import           Data.List

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    getLine >>= putStrLn . show . solve where
        solve [] = 0
        solve (x:xs) = length (takeWhile (== x) xs) + solve (dropWhile (== x) xs)
