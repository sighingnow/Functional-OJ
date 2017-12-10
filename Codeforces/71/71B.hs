-- Codeforces 71B

import Control.Applicative
import Data.List

main :: IO ()
main = do
    [n, k, t] <- (map read . words) <$> getLine :: IO [Int]
    let (u, v) = (t * (n * k) `div` 100) `divMod` k
        r = if u == n
               then replicate u k
               else replicate u k ++ [v] ++ replicate (n - u - 1) 0
    putStrLn $ intercalate " " (map show r)
