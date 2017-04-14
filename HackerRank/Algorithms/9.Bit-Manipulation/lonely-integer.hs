-- HackerRank challenge - Algorithms - 9-1: lonely-integer

import Data.Bits
import Data.List

main :: IO ()
main = do
    _ <- getLine
    xs <- fmap (map read . words) getLine :: IO [Int]
    print . foldl1' xor $ xs
