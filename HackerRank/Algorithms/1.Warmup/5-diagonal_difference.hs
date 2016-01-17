-- HackerRank challenge - Algorithms - 1-5: diagonal difference

import           Control.Monad

main :: IO ()
main = do
    n <- readLn :: IO Int
    mat <- fmap (map (map (read :: String -> Int) . words) . lines) getContents :: IO [[Int]]
    print $ abs (sum [mat!!i!!i | i <- [0..n-1]] - sum [mat!!i!!(n-1-i) | i <- [0..n-1]])
