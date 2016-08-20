module Codewars.Kata.Braces where

validBraces :: String -> Bool
validBraces = helper [] where
  helper [] []     = True
  helper xs []     = False
  helper [] (x:xs) = helper [x] xs
  helper (t:ts) (x:xs)
    | rev t x      = helper ts xs
    | otherwise    = helper (x:t:ts) xs
    where
      rev '(' ')' = True
      rev '[' ']' = True
      rev '{' '}' = True
      rev _ _     = False
