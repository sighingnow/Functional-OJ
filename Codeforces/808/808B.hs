-- Codeforces 808B

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# OPTIONS_GHC -optc-ffast-math #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Data.Int
import Data.List
import Data.Maybe

main :: IO ()
main = do
    (n:k:xs) <- (map (fromIntegral . fst . fromJust . BC.readInt) . BC.words) <$> BC.getContents :: IO [Int64]
    let v = min k (n - k + 1)
        s = sum $ zipWith (\x i -> if | i <= v -> i * x
                                      | i > v && i <= n - v -> v * x
                                      | i > n - v -> (n - i + 1) * x) xs [1..]
        s' = fromIntegral s :: Double
        t' = fromIntegral (n - k + 1) :: Double
    print (s' / t')
