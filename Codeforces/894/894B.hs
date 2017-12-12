-- Codeforces 894B

-- If m + n is odd and k is -1, then ans is 0
-- Otherwise, ans is 2 ^ ((n - 1) * (m - 1))

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -funfolding-use-threshold=16 #-}
{-# OPTIONS_GHC -fexcess-precision #-}
{-# OPTIONS_GHC -feager-blackholing #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fdicts-strict #-}
{-# OPTIONS_GHC -optc-O3 #-}
{-# OPTIONS_GHC -optc-ffast-math #-}

import Control.Applicative
import Data.Int

main :: IO ()
main = do
    [n, m, k] <- (map read . words) <$> getLine :: IO [Int64]
    if odd (m + n) && k == -1
        then print 0
        else print $ fastPowMod (fastPowMod 2 (n - 1) 1000000007) (m - 1) 1000000007

-- | Return (a^b)%m
fastPowMod :: Int64 -> Int64 -> Int64 -> Int64
fastPowMod = go 1 where
    go r _ 0 _ = r
    go r a b m = go r' a' b' m
        where b' = b `div` 2
              r' = if b `mod` 2 == 1
                      then r * a `mod` m
                      else r
              a' = a * a `mod` m

