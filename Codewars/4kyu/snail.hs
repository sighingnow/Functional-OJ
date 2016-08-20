module Snail where

snail :: [[Int]] -> [Int]
snail = reverse . visit 1 [] where
  visit _ acc []   = acc
  visit 1 acc grid = visit 2 (reverse (head grid) ++ acc) (tail grid)
  visit 2 acc grid = visit 3 (reverse (map last grid) ++ acc) (map init grid)
  visit 3 acc grid = visit 4 (last grid ++ acc) (init grid)
  visit 4 acc grid = visit 1 (map head grid ++ acc) (map tail grid)
