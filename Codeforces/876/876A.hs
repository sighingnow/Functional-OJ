-- Codeforces 876A

import Control.Applicative

main :: IO ()
main = do
    [n, a, b, c] <- (map read . words) <$> getContents
    let m = minimum [a, b, c]
    if m == a || m == b || n == 1
        then print $ min a b * (n - 1)
        else print $ min a b + c * (n - 2)
