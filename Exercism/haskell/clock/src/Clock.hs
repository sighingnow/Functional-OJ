module Clock (clockHour, clockMin, fromHourMin, toString) where

data Clock = Clock { hour :: Int
                   , minute :: Int
                   }

instance Eq Clock where
    (Clock h1 m1) == (Clock h2 m2) = h1 == h2 && m1 == m2

instance Show Clock where
    show = toString

instance Num Clock where
    fromInteger n = Clock h m where
        h = (fromIntegral n) `div` 60 `mod` 24
        m = (fromIntegral n) `mod` 60
    negate x = fromInteger (toInteger (Clock 24 0) - toInteger x)
    a + b = fromInteger (toInteger a + toInteger b)

instance Ord Clock where
    compare a b = compare (toInteger a) (toInteger b)

instance Enum Clock where

instance Real Clock where
    toRational = undefined

instance Integral Clock where
    toInteger (Clock h m) = fromIntegral $ h * 60 + m

clockHour :: Clock -> Int
clockHour = hour

clockMin :: Clock -> Int
clockMin = minute

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = fromInteger . fromIntegral $ (h * 60 + m)

toString :: Clock -> String
toString (Clock h m) = disp h ++ ":" ++ disp m
  where disp n | n < 10 = '0' : show n
               | otherwise = show n
