-- Codeforces 450C

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
    [n, m, k] <- (map (fromIntegral . fst . fromJust . BC.readInt) . BC.words) <$> BC.getLine :: IO [Int64]
    let n' = min m n
        m' = max m n
    print $ optimal n' m' k

  where optimal n m k | k < m = max (cnt (1, k + 1)) (cnt (k + 1, 1))
                      | k <= n + m - 2 = cnt (k + 2 - m, m)
                      | otherwise = -1
                      where cnt (x, y) = (n `div` x) * (m `div` y)
