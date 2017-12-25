-- Codeforces 669A

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fexcess-precision #-}
{-# OPTIONS_GHC -fdicts-strict #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -funfolding-use-threshold=16 #-}
{-# OPTIONS_GHC -optc-O3 #-}
{-# OPTIONS_GHC -optc-ffast-math #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    let (u, v) = n `divMod` 3
        r = if v == 0 then 2 * u else 2 * u + 1
    print r
