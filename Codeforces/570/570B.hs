-- Codeforces 570B

main :: IO ()
main = do
    [n, m] <- fmap (map read . words) getLine :: IO [Integer]
    print $ if n == 1 then 1 else if m <= (n `div` 2) then m+1 else m-1
