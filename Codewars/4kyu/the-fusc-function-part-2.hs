module Fusc where

-- ref: https://stackoverflow.com/a/44557597/5080177

fusc :: Integer -> Integer
fusc n
  | n <= 0 = 0
  | otherwise = let a = 1; b = 0 in step n a b
  where
    step n a b
      | n == 0 = b
      | r == 0 = step n' (a+b) b
      | otherwise = step ((n-1) `div` 2) a (a+b)
      where (n', r) = n `divMod` 2
