module DigitalRoot where

digitalRoot :: Integral a => a -> a
digitalRoot n
  | k < 10    = k
  | otherwise = digitalRoot k
  where
    k = sum (trans n)
    trans n = if n < 10 then [n] else (rem n 10) : trans (div n 10)
