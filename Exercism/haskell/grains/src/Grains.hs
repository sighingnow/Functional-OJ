module Grains (square, total) where

square :: Integer -> Maybe Integer
square n | n < 1 = Nothing
         | n == 1 = Just 1
         | n > 64 = Nothing
         | otherwise = (2 *) <$> square (n-1)

total :: Integer
total = 2 ^ 64 - 1
