-- Codeforces 252B

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -funfolding-use-threshold=16 #-}
{-# OPTIONS_GHC -fexcess-precision #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fdicts-strict #-}
{-# OPTIONS_GHC -optc-O3 #-}
{-# OPTIONS_GHC -optc-ffast-math #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Data.Bits
import Data.Maybe
import qualified Data.ByteString.Char8 as BC

readInt :: BC.ByteString -> Int
readInt !s = case BC.readInt s of Just (n, _) -> n

main :: IO ()
main = do
    (x:xs) <- (map readInt . BC.words) <$> BC.getContents :: IO [Int]
    let ys = scanl1 xor (0:xs)
    print $ maximum [x `xor` y | x <- ys, y <- ys]
