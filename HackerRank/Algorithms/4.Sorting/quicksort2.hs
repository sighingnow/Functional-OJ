-- HackerRank challenge - Algorithms - 4-7: quicksort2

import Control.Monad

main :: IO ()
main = do
    _ <- getLine
    xs <- fmap (map read . words) getContents :: IO [Int]
    case xs of
        [] -> return ()
        _ -> void $ go xs
    where
        go (p:vs) = do
            let ls = filter (< p) vs
                rs = filter (> p) vs
            ls' <- if length ls > 1 then go ls else return ls
            rs' <- if length rs > 1 then go rs else return rs
            let r = ls' ++ [p] ++ rs'
            disp r >> return r
        disp = putStrLn . unwords . map show
