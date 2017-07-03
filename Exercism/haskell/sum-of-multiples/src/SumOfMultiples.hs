module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: Integral a => [a] -> a -> a
sumOfMultiples xs limit = sum $ filter (\t -> any ((== 0) . mod t) xs) [1 .. (limit - 1)]
