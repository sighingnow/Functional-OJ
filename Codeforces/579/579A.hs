-- Codeforces 579A

main :: IO ()
main = fmap read getLine >>= print . solve

solve :: Int -> Int
solve 1 = 1
solve n = (n `rem` 2) + solve (n `div` 2)

