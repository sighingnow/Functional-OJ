module Codewars.Kata.Fib where

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Returns a pair of consecutive Fibonacci numbers a b,
--   where (a*b) is equal to the input, or proofs that the
--   number isn't a product of two consecutive Fibonacci
--   numbers.
productFib :: Integer -> (Integer, Integer, Bool)
productFib = grep fibs
  where
    grep (n1:n2:ns) n
      | x == n = (n1, n2, True)
      | x < n = grep (n2:ns) n
      | x > n = (n1, n2, False)
      where x = n1 * n2


