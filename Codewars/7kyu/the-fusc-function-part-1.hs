module Fusc where

fusc :: Int -> Int
fusc 0 = 0
fusc 1 = 1
fusc n
  | r == 0 = fusc n'
  | otherwise = fusc n' + fusc (n' + 1)
  where (n', r) = n `divMod` 2
