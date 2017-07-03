module Bob (responseFor) where

import Data.Char
import Data.List

responseFor :: String -> String
responseFor s
    | all isSpace s = "Fine. Be that way!"
    | any isUpper s && not (any isLower s) = "Whoa, chill out!"
    | last (dropWhileEnd isSpace s) == '?' = "Sure."
    | otherwise = "Whatever."
