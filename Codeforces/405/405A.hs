-- Codeforces 405A

main :: IO ()
main = do
    n <- fmap read getLine
    getLine >>= putStrLn . solve n . map read . words

solve :: Int -> [Int] -> String
solve n xs = foldl (\acc a -> acc ++ (show a) ++ " ") "" $ map (\x -> length $ filter (>= (n+1-x)) k) [1..n] where k = map (\x -> length $ filter (>= x) xs) [1..100]
