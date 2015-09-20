-- Codeforces 282A

import Control.Monad

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    replicateM n getLine >>= putStrLn . show . sum . map solve where
        solve :: [Char] -> Int
        solve [_, '+', _] = 1
        solve [_, '-', _] = -1