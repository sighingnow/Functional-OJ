module Codewars.G964.DblLinear where

dblLinear :: Int -> Integer
dblLinear = (memo !!) where
    merge (x:xs) (y:ys) = case compare x y of
        EQ -> x : merge xs ys
        LT -> x : merge xs (y:ys)
        GT -> y : merge (x:xs) ys
    memo = 1 : merge (map (\x -> 2*x+1) memo) (map (\x -> 3*x+1) memo)
