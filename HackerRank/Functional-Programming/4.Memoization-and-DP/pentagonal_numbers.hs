{-# OPTIONS -O2 #-}
{-# LANGUAGE BangPatterns #-}

import           Data.Array

-- Pentagonal Numbers

stars :: Array Int Int
stars = listArray (1, 100001) xs where xs = 1 : zipWith (+) [4, 7..] xs

star :: Int -> Int
star n = (stars ! n)

main :: IO ()
main = getContents >>= mapM_ (print . star) . tail . map (read :: String -> Int) . words
