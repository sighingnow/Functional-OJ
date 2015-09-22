-- Codeforces 119A

main :: IO ()
main = getLine >>= print . solve 'S' . map read . words where
    solve 'S' [a, b, n] = if n < g then 1 else solve 'A' [a, b, n-g] where g = gcd a n
    solve 'A' [a, b, n] = if n < g then 0 else solve 'S' [a, b, n-g] where g = gcd b n

