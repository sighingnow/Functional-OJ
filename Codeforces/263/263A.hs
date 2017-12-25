-- Codeforces 263A

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O3 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
    mat <- replicateM 5 ((map read . words) <$> getLine) :: IO [[Int]]
    let Just row = findIndex (any (== 1)) mat
        Just column = findIndex (== 1) (mat !! row)
    print $ abs (row + 1 - 3) + abs (column + 1 - 3)
