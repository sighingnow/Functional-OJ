-- | HackerRank challenge - Functional Programming - 2-4 - functions and fractals: sierpinski triangles

blank :: Int -> Int -> [String]
blank k l = replicate k $ replicate l '_'

triangle :: Int -> [String]
triangle 1 = ["1"]
triangle n = tri 1 where
    tri x
        | x > n = []
        | otherwise = tri' : tri (x+1)
        where tri' = replicate (n-x) '_' ++ replicate (2*x-1) '1' ++ replicate (n-x) '_'

extend a = foldl1 (zipWith (++)) [blank p p, a, blank p p] ++ foldl1 (zipWith (++)) [a, blank p 1, a] where p = length a

sierpinski :: Int -> Int -> [String]
sierpinski row 0 = triangle row
sierpinski row n = extend (sierpinski (row `div` 2) (n-1))

main :: IO ()
main = do
    n <- fmap read getLine
    putStr $ unlines $ sierpinski 32 n

