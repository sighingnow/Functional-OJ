module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = primeFactor n 2 where
    primeFactor 1 _ = []
    primeFactor n k = if n `mod` k == 0
                         then k : primeFactor (n `div` k) 2
                         else primeFactor n (k+1)
