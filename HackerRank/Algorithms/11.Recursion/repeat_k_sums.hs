-- HackerRank challenge - Algorithms - 11-11: repeat-k-sums

{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import           Data.Array.IO
import           Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
    t <- read <$> getLine
    replicateM_ t perform

perform :: IO ()
perform = do
    [n, k] <- (map read . words) <$> getLine
    values <- newArray (0, n-1) 0 :: IO (IOUArray Int Int)
    sums <- foldM (\acc _ -> (flip Set.insert acc . read) <$> getLine) Set.empty [1..n]

    foldM_ (eval values k) sums [0..n-1]

    getElems values >>= print

  where
    eval :: IOUArray Int Int -> Int -> Set Int -> Int -> IO (Set Int)
    eval values !k sums !i = do
        let now = Set.elemAt 0 sums
        if i == 0
           then
               writeArray values i (now `div` k)
           else do
               a <- readArray values 0
               let a' = a * (k-1)
               writeArray values i a'
               forM_ [1..k] $ \j ->
                   update values (i-1) (a'*j) (k-j)
        return (Set.deleteAt 0 sums)

update :: IOUArray Int Int -> Int -> Int -> Int -> IO (Set Int)
update values !now !num !less = do
    a <- readArray values 0
    let num' = if now == 0
            then num
            else num + less * a
    unless (now == 0 || less == 0) $
        forM_ [0..less] $ \i -> do
            a' <- readArray values now
            update values (now-1) (num'+i*a') (less-i)

