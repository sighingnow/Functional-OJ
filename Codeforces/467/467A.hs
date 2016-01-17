-- Codeforces 467A

import           Control.Monad

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    replicateM n getLine >>= print . length . filter (\x -> (x!!1)-(x!!0) >= 2) . map (map read . words)
