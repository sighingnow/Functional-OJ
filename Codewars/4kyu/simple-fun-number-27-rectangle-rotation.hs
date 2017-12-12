module RectangleRot.JorgeVS.Kata where

rectangleRot :: Int -> Int -> Int
rectangleRot a b
  | k13 == 0 = s + 4 * (k12 * k21)
  | k13 == 1 = s + 4 * ((k12 + 1) * (k21 - k21 `div` 2) + k12 * (k21 `div` 2))
  where
    t = sqrt 2 / 2
    s = floor (fromIntegral a / 2 / sqrt 2) * 2 + floor (fromIntegral b / 2 / sqrt 2) * 2 + 1
    k11 = floor (fromIntegral a / 2 / t)
    (k12, k13) = k11 `divMod` 2
    k21 = floor (fromIntegral b / 2 / t)
