-- Codeforces 158B

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    getLine >>= putStrLn . show . solve . map read . words where
        solve xs = d + c + (b * 2 + 3 + max 0 (a - c)) `div` 4 where
            [a, b, c, d] = map (\x -> length $ filter (== x) xs) [1 .. 4]

