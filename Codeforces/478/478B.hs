-- Codeforces 478B

main :: IO ()
main = getLine >>= putStrLn . solve . map read . words where
    solve [n, m] = concat [show ((a * (a-1) `div` 2 * (m-b))+(a * (a+1) `div` 2 * b)), " ", show ((n-m+1) * (n-m) `div` 2)] where
        a = n `div` m
        b = n `mod` m
