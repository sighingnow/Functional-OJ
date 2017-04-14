-- HackerRank challenge - Algorithms - 4-8: quicksort3

{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Control.Monad
import Data.Array.IO
import Data.IORef

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    xs <- fmap (map read . words) getLine :: IO [Int]
    arr <- newListArray (1, n) xs :: IO (IOUArray Int Int)
    -- n <- read <$> getLine :: IO Int
    -- xs <- fmap (map read . words) getLine :: IO [Int]
    -- arr <- newListArray (1, n) xs :: IO (IOUArray Int Int)
    quicksort arr 1 n
    disp arr

disp arr = getElems arr >>= putStrLn . unwords . map show

swap :: (Ix i, MArray arr e m) => arr i e -> i -> i -> m ()
swap arr ia ib = do
    a <- readArray arr ia
    b <- readArray arr ib
    writeArray arr ia b
    writeArray arr ib a

foreach :: (Monad a, Foldable t) => t a -> b -> (b -> a -> m b) -> m b
foreach xs v f = foldM f v xs

-- partition :: IOUArray Int Int -> Int -> Int -> Int -> IO Int
partition arr l r mid = do
    p <- readArray arr mid
    swap arr mid r
    slot <- foreach [l..r-1] l (\slot i -> do
        val <- readArray arr i
        if val < pivot
           then swap arr i slot >> return (slot+1)
           else return slot)
    swap arr slot r >> return slot
{-
foreach [l..r-1] l (\slot i -> do
        val <- readArray arr i
        if val < p
           then do
               swap arr i slot
               return $ slot + 1
           else return slot)
    swap arr slot r >> return slot
-}

quicksort :: IOUArray Int Int -> Int -> Int -> IO ()
quicksort arr l r = when (r > l) $ do
    let mid = l + (r-l) `quot` 2
    nxtmid <- partition arr l r mid
    quicksort arr l (nxtmid - 1)
    quicksort arr (nxtmid + 1) r
