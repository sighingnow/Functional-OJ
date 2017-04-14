-- HackerRank challenge - Algorithms - 4-1: tutorial-intro

import Data.List
import Data.Maybe

main :: IO ()
main = do
    v <- fmap read getLine :: IO Int
    n <- fmap read getLine :: IO Int
    xs <- fmap (map read . words) getLine :: IO [Int]
    print (fromJust $ elemIndex v xs)
