-- Codeforces 121A

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

import Control.Applicative
import Control.Monad
import Control.Monad.ST.Safe
import Data.Array.IArray
import Data.Array.MArray.Safe
import Data.Array.ST.Safe
import Data.Foldable
import Data.Int
import Data.List
import Data.STRef

foreach :: Monad m => [a] -> b -> (b -> a -> m b) -> m b
foreach xs v f = foldM f v xs

-- range: [, ], left inclusive and right inclusive
qsort :: (MArray arr e m, Ord e) => arr Int e -> Int -> Int -> m ()
qsort = qsortImpl
  where
    qsortImpl arr l r = when (r > l) $ do
        let mid = l + (r - l) `div` 2
        nmid <- partition arr l r mid
        qsortImpl arr l (nmid - 1)
        qsortImpl arr (nmid + 1) r
    swap arr ia ib = do
        a <- readArray arr ia
        b <- readArray arr ib
        writeArray arr ia b
        writeArray arr ib a
    partition arr l r mid = do
        pivot <- readArray arr mid
        swap arr mid r
        slot <- foreach [l..r-1] l (\slot i -> do
            val <- readArray arr i
            if val < pivot
               then swap arr i slot >> return (slot+1)
               else return slot)
        swap arr slot r >> return slot

main :: IO ()
main = do

    let nlen = 2 + Data.List.sum (map (2 ^) [1..9])

    let arr = runSTUArray $ do
          arr <- newArray_ (0, nlen) :: ST s (STUArray s Int Int64)
          let go k top arr | k > 444444444 = return ()
                           | otherwise = do
                                z <- readSTRef top
                                writeArray arr z (k * 10 + 4)
                                modifySTRef' top (+ 1)
                                go (k * 10 + 4) top arr
                                z <- readSTRef top
                                writeArray arr z (k * 10 + 7)
                                modifySTRef' top (+ 1)
                                go (k * 10 + 7) top arr

          writeArray arr 0 0
          top <- newSTRef 1
          go 0 top arr
          z <- readSTRef top
          qsort arr 1 nlen
          return arr

    let sumAll x | x == 0 = 0
                 | otherwise = go 1
                 where go i | i > nlen = 0
                            | x >= a = a * (a - b) + go (i + 1)
                            | otherwise = a * (x - b)
                            where a = arr ! i
                                  b = arr ! (i - 1)

    [a, b] <- (map read . words) <$> getLine :: IO [Int64]
    print $ sumAll b - sumAll (a - 1)
