module PyramidSlideDown where

dp :: [[Int]] -> Int
dp [[x]] = x
dp (x1:x2:xs) = dp (x3:xs) where x3 = zipWith (+) (zipWith max x1 (tail x1)) x2

longestSlideDown :: [[Int]] -> Int
longestSlideDown [] = 0
longestSlideDown [[]] = 0
longestSlideDown xs = dp . reverse $ xs
