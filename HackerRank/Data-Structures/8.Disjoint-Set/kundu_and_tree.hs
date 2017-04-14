{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

import           Control.Monad
import           Data.Array.IO

main :: IO ()
main = do
    n <- read <$> getLine
    roots <- newListArray (1, n) [1..n] :: IO (IOUArray Int Int)
    counts <- newArray (1, n) 1 :: IO (IOUArray Int Int)
    replicateM_ (n-1) (perform roots counts)
    ns <- forM [1..n] $ \i -> do
        i' <- readArray roots i
        if i == i'
           then readArray counts i
           else return 0
    let n' = fromIntegral n :: Integer
        ns' = map fromIntegral . filter (> 0) $ ns
        t = sum $ map (\x -> x*(x-1)*(3*n'-2*x-2)) ns'
    print $ (n'*(n'-1)*(n'-2) - t) `div` 6 `mod` 1000000007

perform roots counts = do
    [a, b, c] <- words <$> getLine
    case c of
      "r" -> return ()
      "b" -> update (read a) (read b)
  where

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

