module Codewars.Kata.Combos where

combosImpl :: Int -> Int -> [[Int]]
combosImpl k n
  | n == 0 = [[]]
  | k == n = [[n]]
  | k > n = []
  | otherwise = concatMap (\i -> (i :) <$> combosImpl i (n - i)) [k..n]

combos :: Int -> [[Int]]
combos = combosImpl 1
