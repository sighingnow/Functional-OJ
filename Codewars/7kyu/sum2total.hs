module Codewars.Kata.Sum2Total where

total :: Num a => [a] -> a
total xs = head $ foldl (\xs _ -> iter xs) xs [1..(length xs - 1)] where
    iter xs = zipWith (+) xs (tail xs)
