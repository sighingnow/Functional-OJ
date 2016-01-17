-- Codeforces 260A

-- At first try to add to the right one digit from 0 to 9.
-- If it is impossible write -1.
-- In other case, the remaining n–1 digits can be 0 because divisibility doesn’t change.

main :: IO ()
main = getLine >>= putStrLn . solve . map read . words

solve :: [Int] -> String
solve [a, b, n] = if null k then "-1" else show (head k) ++ replicate (n-1) '0'
    where
        k = filter (\x -> x `mod` b == 0) $ map (a * 10 +) [0..9]

