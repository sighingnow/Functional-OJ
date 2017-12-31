-- Codeforces 797A

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O3 #-}
{-# OPTIONS_GHC -optc-ffast-math #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Data.List

main :: IO ()
main = do
    [n, k] <- (map read . words) <$> getLine
    let xs = go n (k - 1) 2 0
    if length xs < k
       then print (-1)
       else putStrLn . unwords . map show $ xs
  where
    go n k n' k'
       | k == k' || n <= n' = [n]
       | b == 0 = n' : go a k n' (k' + 1)
       | otherwise = go n k (n' + 1) k'
       where (a, b) = n `divMod` n'
