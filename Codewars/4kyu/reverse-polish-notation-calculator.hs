module RPN where

import Data.Char

calc :: String -> Double
calc = parse [] . words where
  parse xs [] = if xs == [] then 0 else last xs
  parse xs (t:ts) = if isDigit (head t)
    then parse (xs ++ [t']) ts
    else parse ((init $ init xs) ++ [n]) ts
    where
      t' = read t :: Double
      a = last (init xs)
      b = last xs
      n = case t of
        "+" -> a + b
        "-" -> a - b
        "*" -> a * b
        "/" -> a / b
