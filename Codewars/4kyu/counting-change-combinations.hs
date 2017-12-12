module Change where

count :: [Int] -> [Integer]
count = foldr acc (1 : repeat 0)
  where
    acc c cs = zs where zs = take c cs ++ zipWith (+) zs (drop c cs)

countChange :: Integer -> [Integer] -> Integer
countChange x xs = count (fromIntegral <$> xs) !! (fromIntegral x)
