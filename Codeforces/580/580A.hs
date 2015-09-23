-- Codeforces 580A

main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Int] -> Int
solve (n:xs) = (\(ans, _, _) -> ans) $ foldl (\(ans, acc, prev) x -> if x >= prev then ((max ans (acc+1)), acc+1, x) else (ans, 1, x)) (0, 0, 0) xs
