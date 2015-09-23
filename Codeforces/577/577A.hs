-- Codeforces 577A

main :: IO ()
main = getLine >>= print . solve . map read . words where
    solve [n, x] = sum [if (x `mod` k == 0) && (x `div` k <= n) then 1 else 0 | k <- [1..n]]
