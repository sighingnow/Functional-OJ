-- HackerRank challenge - Algorithms - 3-3: pangrams

import Data.Char

main :: IO ()
main = do
    s <- fmap (map toLower) getLine
    putStr $ case all (flip elem s ) ['a' .. 'z'] of
         True -> "pangram"
         False -> "not pangram"
