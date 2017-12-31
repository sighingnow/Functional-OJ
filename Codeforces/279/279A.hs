-- Codeforces 279A

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Data.List

main :: IO ()
main = do
    [a, b] <- (map read . words) <$> getLine :: IO [Int]
    print $ go 0 (0, 0) (a, b)
  where
    go !k !(!x, !y) !(!a, !b) = let (x', y') = next (x, y)
                                 in if onLine (x, y) (x', y') (a, b)
                                       then k
                                       else go (k + 1) (x', y') (a, b)
      where next (0, 0) = (1, 0)
            next (1, 0) = (1, 1)
            next !(!x, !y) | x > 0 && y > 0 = (-x, y)
                           | x > 0 && y < 0 = (x, 1 - y)
                           | x < 0 && y > 0 = (x, -y)
                           | x < 0 && y < 0 = (1 - x, y)
    onLine !(!x1, !y1) !(!x2, !y2) !(!a, !b) = (x1 <= a && a <= x2 || x2 <= a && a <= x1) && (y1 <= b && b <= y2 || y2 <= b && b <= y1)
