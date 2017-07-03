module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode [] = []
decode xs
    | null ns = c : decode cs
    | otherwise = replicate (read ns) c ++ decode cs
    where ns = takeWhile isNumber xs
          (c:cs) = dropWhile isNumber xs

encode :: String -> String
encode [] = []
encode [c] = [c]
encode xs@(a:b:_)
    | a == b = (show . length) (takeWhile (== a) xs) ++ [a] ++ encode (dropWhile (== a) xs)
    | otherwise = a : encode (tail xs)
