-- Codeforces 371A

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Data.List

main :: IO ()
main = do
    (n:k:xs) <- (map read . words) <$> getContents
    print . sum . map minimize . groupd k (n `div` k) $ xs
  where
    minimize xs = let (a, b) = foldl' (\(a, b) x -> if x == 1
                                                       then (a + 1, b)
                                                       else (a, b + 1)) (0, 0) xs
                   in min a b
    groupd k h xs = [[ xs !! (i * k + j) | i <- [0 .. (h - 1)] ] | j <- [0 .. (k - 1)]]
