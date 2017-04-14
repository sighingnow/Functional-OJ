-- HackerRank challenge - Algorithms - 4-3: insertionsort2

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Data.List
import Data.Maybe
import Data.Array.IO

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    xs <- fmap (map read . words) getLine :: IO [Int]
    arr <- newListArray (1, n) xs :: IO (IOUArray Int Int)
    insertion arr n 2

display arr = getElems arr >>= putStrLn . unwords . map show

insertion arr n k = do
    a <- readArray arr k
    shift arr n k k a
    display arr
    when (k < n) $ insertion arr n (k+1)

findMin arr n k = do
    if k < n
       then do
           v1 <- readArray arr k
           v2 <- readArray arr (k+1)
           if v1 <= v2
              then findMin arr n (k+1)
              else return (k+1)
       else return (k+1)

shift arr n k p a = do
    if p > 1
       then do
           b <- readArray arr (p-1)
           if b > a
              then do
                  writeArray arr p b
                  when (p > 1) (shift arr n k (p-1) a)
              else do
                  writeArray arr p a
       else do
           writeArray arr p a
