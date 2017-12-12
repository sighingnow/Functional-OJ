-- Codeforces 9A

import Text.Printf

main :: IO ()
main = do
    [a, b] <- fmap (map read . words) getLine :: IO [Int]
    let x = 7 - max a b
        y = gcd x 6
    printf "%d/%d" (x `div` y) (6 `div` y)
