module Codewars.Kata.RemovNB where

import Data.List

removNb :: Integer-> [(Integer, Integer)]
removNb n = sort $ concat $ map fn [mina..n] where
  s = div (n * (n + 1)) 2
  mina = div s n
  fn x = if b == 0 then [(x, a)] else []
      where (a, b) = divMod (s-x) (x+1)
