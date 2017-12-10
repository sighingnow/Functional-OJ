-- Codeforces 32C

import Control.Applicative

main :: IO ()
main = do
    [m, n, s] <- (map read . words) <$> getLine :: IO [Integer]
    let cnt x = if x <= s
                   then x
                   else (u + 1) * (v + 1) where (u, v) = (x - 1) `divMod` s
    print $ cnt m * cnt n
