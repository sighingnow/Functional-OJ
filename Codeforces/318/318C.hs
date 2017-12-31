-- Codeforces 317C

{-# OPTIONS_GHC -O3 #-}
{-# OPTIONS_GHC -optc-O3 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

import Control.Applicative

main :: IO ()
main = do
    [x, y, m] <- (map read . words) <$> getLine :: IO [Integer]
    print $ if | x >= m || y >= m -> 0
               | x <= 0 && y <= 0 -> -1
               | x >= 0 && y >= 0 -> go 0 x y m
               | x <= 0 && y > 0  -> gopre x y m
               | x > 0  && y <= 0 -> gopre y x m
  where
    go k x y m | x >= m || y >= m = k
               | x >= y = go (k + 1) x (x + y) m
               | otherwise = go (k + 1) (x + y) y m
    gopre x y m = let a = (abs x + y - 1) `div` y
                   in a + go 0 (x + a * y) y m
