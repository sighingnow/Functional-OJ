module Codewars.StringMaker where

makeString :: String -> String
makeString = map head . words
