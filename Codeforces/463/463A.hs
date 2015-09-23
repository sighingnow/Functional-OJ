-- Codeforces 463A

main :: IO ()
main = do
    [n, s] <- fmap (map read . words) getLine
    getContents >>= print . solve s . map (map read . words) . lines

solve :: Int -> [[Int]] -> Int
solve s xs = if length enough == 0 then (-1) else 100 - (minimum $ (100:) $ filter (/= 0) $ map (!!1) enough)  where enough = filter (\[a,b] -> a<s || a==s && b==0) xs
                                                                -- prepend 100: to avoid empty list error.
