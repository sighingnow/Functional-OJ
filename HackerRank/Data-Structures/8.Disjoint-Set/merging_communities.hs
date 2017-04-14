{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

import           Control.Monad
import           Data.Array.IO

main :: IO ()
main = do
    [n, q] <- (map read . words) <$> getLine
    roots <- newListArray (1, n) [1..n] :: IO (IOUArray Int Int)
    counts <- newArray (1, n) 1 :: IO (IOUArray Int Int)
    replicateM_ q (perform n roots counts)

perform n roots counts = do
    (c:cs) <- words <$> getLine
    case c of
      "Q" -> answer (read (cs !! 0))
      "M" -> update (read (cs !! 0)) (read (cs !! 1))
  where

    answer x = findRoot x >>= readArray counts >>= print

    update a b = do
        a' <- findRoot a
        b' <- findRoot b
        when (a' /= b') $ do
            writeArray roots a' b'
            c1 <- readArray counts a'
            c2 <- readArray counts b'
            writeArray counts b' (c1+c2)

    findRoot x = do
        x' <- readArray roots x
        if x == x'
           then return x
           else do
               writeArray roots x x'
               findRoot x'

