-- Codeforces 26A

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Data.List

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    print . length . filter almostp $ [1..n]
  where
    almostp !x = let go !k !c !v !x | c >= 3 = False
                                    | x == 1 = c == 2
                                    | x `mod` k == 0 = if v then go k c True (x `div` k)
                                                            else go k (c + 1) True (x `div` k)
                                    | otherwise = go (k + 1) c False x 
                  in go 2 0 False x
