module Green where

import Data.List (find, isSuffixOf)
import Data.Maybe

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
 | x <= y = x : merge xs (y:ys)
 | otherwise = y : merge (x:xs) ys

{-# INLINE merge #-}

valid :: Integer -> Bool
valid x = isSuffixOf (show x) (show (x * x))

{-# INLINE valid #-}

greenImpl :: [Integer]
greenImpl = 0 : 1 : merge (iterate fn 5) (iterate fn 6)
  where
    fn x = let x' = read ('1' : show x) in fromJust $ find valid $ tail [x, x' ..]

green :: Int -> Integer
green = (!!) greenImpl
