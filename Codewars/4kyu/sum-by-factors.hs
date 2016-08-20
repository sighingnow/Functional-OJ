module Codewars.Kata.SumByFactors where

import Data.List

sumOfDivided :: [Integer] -> [(Integer, Integer)]
sumOfDivided xs = map (\p -> (p, sum . filter (\x -> mod x p == 0) $ xs)) primes where
    factor acc p n 
        | n == 1       = acc
        | mod n p == 0 = factor (p:acc) p (div n p)
        | p * p > n    = factor acc n n    -- n is a prime number.
        | otherwise    = factor acc (p+1) n
    primes = sort . nub . concat . map (factor [] 2 . abs) $ xs
