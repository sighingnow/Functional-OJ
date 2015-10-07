-- Hankrank challenge - algorithm - 1-4: a very big sum

import Control.Monad

main :: IO ()
main = do
    n <- readLn :: IO Int
    print . sum . map (read :: String -> Integer) . words =<< getLine