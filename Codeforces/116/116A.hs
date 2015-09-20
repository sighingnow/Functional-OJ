-- Codeforces 116A

import Control.Monad

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    replicateM n getLine >>= putStrLn . show . solve . map (map read . words) where
        solve = fst . foldl (\(m, acc) n -> ((max m (acc+n)), acc+n)) (0, 0) . map (\x -> x!!1 - x!!0)
