-- Codeforces 50A

main :: IO()
main = getLine >>= print . (`div` 2) . product . map read . words

