-- Codeforces 13A

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Data.List

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    let s = sum $ map (digitSum n) [2..n-1]
        k = gcd (n - 2) s
    putStrLn . concat $ [show (s `div` k), "/", show ((n - 2) `div` k)]
  where
    digitSum !n !x | n < x = n
                   | otherwise = let !(!a, !b) = n `divMod` x
                                  in b + digitSum a x
