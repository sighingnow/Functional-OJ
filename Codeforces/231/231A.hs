-- Codeforces 231A

import           Control.Monad

main :: IO ()
main = do
    n <- fmap read getLine
    replicateM n getLine >>= putStrLn . show . solve . map words where
        solve = length . filter (>= 2) . map (sum . map read)
