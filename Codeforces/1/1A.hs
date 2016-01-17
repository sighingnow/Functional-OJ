-- Codeforces 1A

main :: IO()
main = getLine >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve [n, m, a] = ((quot (n-1) a) + 1) * ((quot (m-1) a) + 1)

