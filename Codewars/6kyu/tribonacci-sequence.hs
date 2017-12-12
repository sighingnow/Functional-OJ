module Tribonacci where

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n = take n tribonacciGen
  where
    tribonacciGen = a : b : c : zipWith (+) tribonacciGen (zipWith (+) (tail tribonacciGen) (tail . tail $ tribonacciGen))

