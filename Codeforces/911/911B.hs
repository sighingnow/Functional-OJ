-- Codeforces 911B

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Maybe

main :: IO ()
main = do
    [n, a, b] <- (map (fst . fromJust . BC.readInt) . BC.words) <$> BC.getLine :: IO [Int]
    print . maximum . map (\i -> min (a `div` i) (b `div` (n - i))) $ [max (n-b) 1 .. min (n - 1) a]
