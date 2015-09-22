-- Codeforces 451A

main :: IO ()
main = getLine >>= putStrLn . solve . map read . words where solve [n, m] = ["Malvika", "Akshat"]!!((min n m) `mod` 2)
