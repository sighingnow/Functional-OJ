-- Codeforces 166E

import Data.List
import Debug.Trace

-- Algorithm: matrix exponentiating by squaring

    --[A_n]       [A_{n-1}]     [ 0 1 1 1 ]
    --[B_n]   =   [B_{n-1}]  *  [ 1 0 1 1 ]
    --[C_n]       [C_{n-1}]     [ 1 1 0 1 ]
    --[D_n]       [D_{n-1}]     [ 1 1 1 0 ]

    -- Initial vector: [1, 1, 1, 0]

main :: IO ()
main = getLine >>= print . solve . read

solve :: Integer -> Integer
solve n = quickpowmod keymat n modbase (3,3)

modbase = 10^9 + 7

idmat = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]

keymat = [[0, 1, 1, 1], [1, 0, 1, 1], [1, 1, 0, 1], [1, 1, 1, 0]]

-- quickpowmod mat n m (i, j): calculate mat^n [i,j] % m
quickpowmod :: [[Integer]] -> Integer -> Integer -> (Int, Int) -> Integer
quickpowmod mat n m (i, j) = (loop n idmat mat m) !! i !! j where 
    loop 0 ans _ _ = ans
    loop n ans inter m = loop (n `div` 2) (if n `mod` 2 == 1 then multi ans inter m else ans) (multi inter inter m) m 

multi :: [[Integer]] -> [[Integer]] -> Integer -> [[Integer]]
multi a b m = [[(foldl1' (+) $ zipWith (*) x y) `mod` m | y <- transpose b] | x <- a]
