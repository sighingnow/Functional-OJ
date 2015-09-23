-- Codeforces 459C

main :: IO ()
main = getLine >>= putStrLn . solve . map read . words

solve :: [Int] -> String
solve [n, k, d]

