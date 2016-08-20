module ExplosiveSum where

explosiveSum :: Integer -> Integer
explosiveSum n
  | n < 0     = 0
  | n == 0    = 1
  | otherwise = memo !! (fromIntegral n) !! (fromIntegral n) where
    helper x k
        | x == 1 = 1
        | k == 1 = 1
        | x < k  = memo !! x !! x
        | x == k = 1 + memo !! x !! (k-1)
        | x > k  = memo !! x !! (k-1) + memo !! (x-k) !! k
    memo = [[helper i j | j <- [0..]] | i <- [0..]]
