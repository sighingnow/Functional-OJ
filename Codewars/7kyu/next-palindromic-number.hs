module Codewars.Palindromes where

nextPal :: Int -> Int
nextPal n = head $ dropWhile (not . ispal) [n+1..]
  where ispal n = sn == reverse sn where sn = show n
