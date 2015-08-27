-- Codeforces 4A

main :: IO()
main = readLn >>= putStrLn . solve

solve :: Integer -> [Char]
solve n
    | n `mod` 2 == 0 && n /= 2 = "YES"
    | otherwise = "NO"

