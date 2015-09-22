-- Codeforces 572A

main :: IO ()
main = do
    [na, nb] <- readAndSplit
    [k, m] <- readAndSplit
    a <- readAndSplit
    b <- readAndSplit
    putStrLn $ if (a!!(k-1)) < (b!!(nb-m)) then "YES" else "NO" where
        readAndSplit = fmap (map read . words) getLine
