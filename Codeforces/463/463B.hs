-- Codeforces 463B

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve = fst . foldl (
    \(m, (e, h)) b -> if (e < b-h)
        then (m+(b-h-e), (0, b))
        else (m, (e-(b-h), b)))
    (0, (0, 0))
