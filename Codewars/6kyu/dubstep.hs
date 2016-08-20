module Codewars.Kata.Dubstep where

import Data.List

songDecoder :: String -> String
songDecoder = intercalate " " . words . helper where
  helper (a:b:c:xs)
    | a:b:c:[] == "WUB" = ' ' : helper xs
    | otherwise         = a : helper (b:c:xs)
  helper xs = xs
