-- Hankrank challenge - algorithm - 1-2: solve-me-second

import Control.Monad

main :: IO ()
main = do
    n <- readLn :: IO Int
    mapM_ print . map (sum . map read . words) =<< replicateM n getLine