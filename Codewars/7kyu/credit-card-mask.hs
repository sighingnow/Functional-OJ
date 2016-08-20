module Maskify where

maskify :: String -> String
maskify (a:b:c:d:e:xs) = '#':maskify (b:c:d:e:xs)
maskify xs = xs
