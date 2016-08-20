module Zeros where

zeros :: Int -> Int
zeros n = sum bs where
    k  = decompose n 0
    bs = map (\x -> div n (5^x)) [1..k]

    decompose 0 x = x
    decompose n x = decompose (div n 5) (x+1)
