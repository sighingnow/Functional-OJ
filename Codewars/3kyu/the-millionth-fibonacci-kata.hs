module Fibonacci where

newtype Mat a = Mat { runMat :: (a, a, a, a) }

instance Integral a => Num (Mat a) where
  Mat (a1, b1, c1, d1) + Mat (a2, b2, c2, d2) =
    Mat (a1 + a2, b1 + b2, c1 + c2, d1 + d2)
  Mat (a1, b1, c1, d1) - Mat (a2, b2, c2, d2) =
    Mat (a1 - a2, b1 - b2, c1 - c2, d1 - d2)
  Mat (a1, b1, c1, d1) * Mat (a2, b2, c2, d2) =
    Mat (a1 * a2 + b1 * c2, a1 * b2 + b1 * d2, c1 * a2 + d1 * c2, c1 * b2 + d1 * d2)
  fromInteger n = Mat (fromInteger n, 0, 0, fromInteger n)
  abs (Mat (a1, b1, c1, d1)) = Mat (abs a1, abs b1, abs c1, abs d1)
  signum (Mat (a1, b1, c1, d1)) = Mat (signum a1, signum b1, signum c1, signum d1)

fib :: Integer -> Integer
fib n
  | n < 0 = let Mat (_, v, _, _) = Mat (0, 1, 1, -1) ^ (- n) in v
  | n == 0 = 0
  | otherwise = let Mat (v, _, _, _) = Mat (1, 1, 1, 0) ^ (n - 1) in v
