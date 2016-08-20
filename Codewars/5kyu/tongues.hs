module Codewars.Exercise.Tongues where

import Data.Char

tongues :: String -> String
tongues = map solve where

  solve c
      | isAlpha c = (!! t) $ dropWhile (/= c) xs
      | otherwise = c
      where (t, xs) = head $ filter (elem c . snd) [(3, lvowel), (10, lconsonants), (3, uvowel), (10, uconsonants)]
  lvowel      = concat $ replicate 2 (reverse "aiyeou")
  lconsonants = concat $ replicate 2 (reverse "bkxznhdcwgpvjqtsrlmf")
  uvowel      = map toUpper lvowel
  uconsonants = map toUpper lconsonants
