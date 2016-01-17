-- Codeforces 583B

main :: IO ()
main = getLine >> getLine >>= print . solve 0 0 [] . map read . words

solve :: Int -> Int -> [Int] -> [Int] -> Int
solve c _ [] [] = c
solve c acc left [] = solve (c+1) acc [] left
solve c acc left (x:right)
    | x <= acc = solve c (acc+1) left right
    | otherwise = solve c acc (x:left) right
