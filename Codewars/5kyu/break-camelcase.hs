module Codewars.Kata.BreakCamelCase where

import Data.Char

solution :: String -> String
solution = unwords . words . helper where
  helper (x:xs)
    | isUpper x = ' ' : x : helper xs
    | otherwise = x : helper xs
  helper xs     = xs
