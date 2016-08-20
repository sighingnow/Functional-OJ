module Codewars.Exercise.Pronic where

isPronic :: Integer -> Bool
isPronic k = sk * (sk + 1) == k where sk = (floor . sqrt . fromInteger) k
