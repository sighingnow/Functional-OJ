module Codewars.Kata.Rule30 where

rule30 :: [Int] -> Int -> [Int]
rule30 cells n
    | n < 0     = cells
    | otherwise = transfer cells' n
    where cells' = preprocess cells

transfer xs 0  = xs
transfer [0] n = replicate (1+2*n) 0
transfer [1] n = transfer [1, 1, 1] (n-1)
transfer xs n  = transfer (map iter [1..length xs+2]) (n-1)
    where
        translate [0, 0, 0] = 0
        translate [0, 0, 1] = 1
        translate [0, 1, 0] = 1
        translate [0, 1, 1] = 1
        translate [1, 0, 0] = 1
        translate [1, 0, 1] = 0
        translate [1, 1, 0] = 0
        translate [1, 1, 1] = 0
        xs' = [0, 0]++xs++[0, 0]
        iter k = translate [xs'!!(k-1), xs'!!k, xs'!!(k+1)]

preprocess = map (\x -> if x == 1 then 1 else 0)
