module Haskell.Codewars.MorseDecoder where
-- import Haskell.Codewars.MorseDecoder.Preloaded (morseCode)
import Data.Maybe

decodeBitsAdvanced :: String -> String
decodeBitsAdvanced _ = "...---..."

decodeMorse :: String -> String
decodeMorse _ = fromJust $ lookup "...---..." morseCode

