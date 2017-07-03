module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance [] [] = return 0
distance [] _ = Nothing
distance _ [] = Nothing
distance (x:xs) (y:ys) = do
    let r = if x == y then 0 else 1
    rs <- distance xs ys
    return (r + rs)
