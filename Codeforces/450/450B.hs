-- Codeforces 450B

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
    [x, y, n] <- (map (fromIntegral . fst . fromJust . BC.readInt) . BC.words) <$> BC.getContents :: IO [Int64]
    print $ ([x, y, y-x, -x, -y, x-y] !! (fromIntegral ((n - 1) `mod` 6))) `mod` 1000000007
