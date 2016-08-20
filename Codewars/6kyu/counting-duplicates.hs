module Codwars.Kata.Duplicates where
import Data.List
import Data.Char

duplicateCount :: String -> Int
duplicateCount = length . filter (>1) . map length . group . sort . map toLower
