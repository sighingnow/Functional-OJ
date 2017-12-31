-- Codeforces 686D

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Control.Monad.ST.Safe
import qualified Data.ByteString.Char8 as BC
import Data.Array.IArray
import Data.Array.MArray.Safe
import Data.Array.ST.Safe
import Data.Array.Unboxed
import Data.Foldable
import Data.List
import Data.Maybe

main :: IO ()
main = do
    (n:q:rest) <- (map (fst . fromJust . BC.readInt) . BC.words) <$> BC.getContents
    let !(!xs, !qs) = splitAt (n - 1) rest

    -- build graph
    let !graph = runSTArray $ do
          graph <- newArray (1, n) [] :: ST s (STArray s Int [Int])
          for_ (zip [2..n] xs) $ \(i, x) -> do
              xs <- readArray graph x
              writeArray graph x (i:xs)
          return graph

    let !fa = listArray (2, n) xs :: UArray Int Int

    -- dfs
    let !centeroid = runSTUArray $ do
          son <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
          centeroid <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
          dfs graph fa son centeroid 1
          return centeroid

    -- result
    BC.putStrLn . BC.intercalate "\n" . map (BC.pack . show . (!) centeroid) $ qs

  where

    dfs graph fa son centeroid u = do
        writeArray son u 1
        writeArray centeroid u u

        maxs <- forM (graph ! u) $ \v -> do
            dfs graph fa son centeroid v
            !szu <- readArray son u
            !szv <- readArray son v
            writeArray son u (szu + szv)
            return (szv, v)

        let !maxv = if null maxs then 0
                                 else snd . Data.List.maximum $ maxs

        if maxv /= 0
            then do
                a <- readArray son maxv
                b <- readArray son u
                if a * 2 > b
                    then do
                        su <- readArray son u
                        r <- readArray centeroid maxv
                        r' <- goup fa son su r
                        writeArray centeroid u r'
                    else return ()
            else return ()

    goup fa son su now = do
        k <- readArray son now
        if (su - k) * 2 > su
            then goup fa son su (fa ! now) -- go up
            else return now
