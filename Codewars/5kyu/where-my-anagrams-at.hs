module Anagram where
import Data.List

anagrams :: String -> [String] -> [String]
anagrams w ws = filter ((sort w ==) . sort) ws
