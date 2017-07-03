module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSums n - sumOfSquares n

squareOfSums :: Integral a => a -> a
squareOfSums = (\x -> x * x) . sum . flip take [1..] . fromIntegral

sumOfSquares :: Integral a => a -> a
sumOfSquares = sum . map (\x -> x * x) . flip take [1..] . fromIntegral
