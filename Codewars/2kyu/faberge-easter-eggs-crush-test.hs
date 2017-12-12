module Faberge where

heigth :: Integer -> Integer -> Integer
heigth 0 _ = 0
heigth _ 0 = 0
heigth n m
  | n > m = heigth m m
  | otherwise = go 1 1
  where
    go b c
      | c > n = 0
      | otherwise = let d = b * (m - c + 1) `div` c in d + go d (c + 1)
