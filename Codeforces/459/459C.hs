-- Codeforces 459C

import           Data.List

main :: IO ()
main = getLine >>= putStrLn . solve . map read . words

solve :: [Int] -> String
solve [n, k, d] = if check $ map toInteger [n, k, k, d]
    then "-1"
    else intercalate "\n" $ map (intercalate " " . map show) $ transpose $ scanl (\acc _ -> inc acc) (replicate d 1) [1..n-1] where inc = tail . map snd . scanl (\(cin, _) x -> if x+cin > k then (1, x+cin-k) else (0, x+cin)) (1, 0)

check :: [Integer] -> Bool
check [n, _, _, 0] = True
check [n, acc, k, d] = if acc >= n then False else check [n, (k*acc), k, (d-1)]
