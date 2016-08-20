module Codewars.Kata.DigPow where

digpow :: Integer -> Integer -> Integer
digpow n p = if rem s n == 0 then div s n else (-1) where
  splitn 0 = []
  splitn n = splitn (div n 10) ++ [(rem n 10)]
  s = sum $ zipWith (^) (splitn n) [p..]
