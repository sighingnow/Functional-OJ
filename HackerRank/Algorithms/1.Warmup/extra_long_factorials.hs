-- HackerRank challenge - Algorithms - 1-10: extra long factorials

import           Data.List (foldl')

main :: IO ()
main = readLn >>= print . solve where
    solve n = foldl' (*) 1 [1..n]
