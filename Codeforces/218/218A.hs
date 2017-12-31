-- Codeforces 218A

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Applicative
import Control.Monad.ST.Safe
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Maybe

main :: IO ()
main = do
    [n, k] <- (map read . words) <$> getLine
    xs <- (map (fst . fromJust . BC.readInt) . BC.words) <$> BC.getContents
    go xs 0 k
  where
    go xs i k | i == k = putStr (intercalate " " (map show xs))
    go (a:b:c:xs) i k = do
        let peak = a + 1 < b && c + 1 < b
        putStr (show a)
        putStr " "
        putStr (show $ if peak then (b - 1) else b)
        putStr " "
        go (c:xs) (if peak then (i + 1) else i) k
