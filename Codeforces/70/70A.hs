-- Codeforces 70A

{-# OPTIONS_GHC -O2 -optc-O2 #-}

import Control.Applicative

main :: IO ()
main = do
    x <- read <$> getLine :: IO Int
    if x == 0
       then print 1
       else print $ powWithMod 3 (x - 1) (1000000 + 3) 
  where
    powWithMod x n m = go n 1 x m
      where go 0 r _ _ = r
            go n r x m = let n' = n `div` 2
                             r' = if n `mod` 2 == 0
                                     then r
                                     else r * x `mod` m
                             x' = x * x `mod` m
                          in go n' r' x' m
