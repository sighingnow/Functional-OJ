-- Codeforces 802A

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -funfolding-use-threshold=16 #-}
{-# OPTIONS_GHC -fexcess-precision #-}
{-# OPTIONS_GHC -feager-blackholing #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fdicts-strict #-}
{-# OPTIONS_GHC -optc-O3 #-}
{-# OPTIONS_GHC -optc-ffast-math #-}

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BC

readInts :: IO [Int]
readInts = map (fst . fromJust . BC.readInt) . BC.words <$> BC.getLine

main :: IO ()
main = do
    [n, k] <- (map read . words) <$> getLine :: IO Int
    ns <- readInts
