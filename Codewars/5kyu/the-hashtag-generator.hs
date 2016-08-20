module Codewars.Kata.Hashtag where

import Data.Char

generateHashtag :: String -> Maybe String
generateHashtag xs
  | len == 0 || len > 140 = Nothing
  | otherwise = Just (('#' :) $ concat $ map trans (words xs))
  where
    len = (length . unwords . words) xs
    trans (t:ts) = toUpper t : ts
