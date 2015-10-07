-- Hankrank challenge - algorithm - 1-3: simple array sum

import Control.Monad

main :: IO ()
main = do
    n <- readLn :: IO Int
    print . sum . map read . words =<< getLine