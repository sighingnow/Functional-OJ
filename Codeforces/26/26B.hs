-- Codeforces 26B

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Data.List

main :: IO ()
main = let match !(!a, !b) '(' = (a, b + 1)
           match !(!a, 0) ')' = (a, 0)
           match !(!a, !b) ')' = (a + 2, b - 1)
        in getLine >>= print . fst . foldl' match (0, 0)
