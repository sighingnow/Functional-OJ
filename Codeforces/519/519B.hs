-- Codeforces 519B

import           Control.Monad

main :: IO ()
main = do
    n <- fmap read getLine :: IO Integer
    fmap (map read . words) getLine >>= solve 2

solve :: Integer -> [Integer] -> IO ()
solve 0 _ = return ()
solve n xs = do
    nxt <- fmap (map read . words) getLine
    print ((sum xs) - (sum nxt))
    solve (n-1) nxt
