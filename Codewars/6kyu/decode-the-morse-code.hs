module Codewars.Kata.DecodeMorse (decodeMorse) where

import Codewars.Kata.DecodeMorse.Preload (morseCodes)

import Data.List
import Data.Map.Strict ((!))

-- morseCodes = []

space :: Char -> Bool
space = (==) ' '

{-# INLINE space #-}

nspace :: Char -> Bool
nspace = (/=) ' '

{-# INLINE nspace #-}

splitS :: String -> (String, String)
splitS s
  | s' == [] = ("", "")
  | otherwise = let l = takeWhile nspace s'
                    r = drop (length l) s'
                 in (l, r)
  where s' = dropWhile space s

{-# INLINE splitS #-}

matchSep :: String -> Bool
matchSep = (== 3) . length . takeWhile space

{-# INLINE matchSep #-}

decodeImpl :: String -> String
decodeImpl [] = []
decodeImpl s =
  let (l, r) = splitS s
      l' = morseCodes ! l
   in if matchSep r
         then l' ++ " " ++ decodeImpl r
         else l' ++ decodeImpl r

{-# INLINE decodeImpl #-}

decodeMorse :: String -> String
decodeMorse s = decodeImpl (dropWhileEnd space (dropWhile space s))
