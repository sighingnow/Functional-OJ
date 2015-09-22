-- Codeforces 519C

main :: IO ()
main = getLine >>= print . solve . map read . words where
    solve [n, m] = foldl (\a i -> max a (i+(min (n-2*i) ((m-i) `div` 2)))) 0 [0..(min m (n `div` 2))]
