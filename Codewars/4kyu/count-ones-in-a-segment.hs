module CountOnes where

-- ref: http://www.cnblogs.com/jy02414216/archive/2011/03/09/1977724.html

countRec :: Int -> Int -> Int
countRec n k
    | n `div` k == 0 = 0
    | otherwise = case value of
                    0 -> higher * k + countRec n (k * 2)
                    1 -> higher * k + lower + 1 + countRec n (k * 2)
    where
        higher = n `div` (k * 2)
        lower = n - (n `div` k) * k
        value = n `div` k `mod` 2

countInt :: Int -> Int
countInt n = countRec n 1

{-# INLINE countInt #-}

countOnes :: Integer -> Integer -> Integer
countOnes left right = fromIntegral $ countInt (fromIntegral right) - countInt (fromIntegral (left - 1))
