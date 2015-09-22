-- Codeforces 82A

-- use math method to solve this problem.

main :: IO ()
main = getLine >>= putStrLn . (["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"] !!) . solve 1 . (+ (-1)) .read where
    solve p n
        | n < p * 5 = n `div` p
        | otherwise = solve (p*2) (n-p*5)
