{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

import           Control.Monad
import           Data.Array.IO
import           Data.Function
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe

main :: IO ()
main = do
    [n, q] <- (map read . words) <$> getLine
    roots <- newListArray (1, n) [1..n] :: IO (IOUArray Int Int)
    counts <- newArray (1, n) 1 :: IO (IOUArray Int Int)

    edges <- foldM readEdge Map.empty [1..n-1]
    (rs, _) <- foldM
            (\(rs, r) (w, uvs) -> do
                total <- ((r +) . sum) <$> mapM (perform roots counts) uvs
                return (Map.insert w total rs, total))
            (Map.empty, 0) (Map.toAscList edges)

    replicateM_ q $ do
        [l, r] <- (map read . words) <$> getLine
        let (_, vl) = fromMaybe (0, 0) $ Map.lookupLT l rs
            (_, vr) = fromMaybe (0, 0) $ Map.lookupLE r rs
        print $ vr - vl

readEdge edges _ = do
    [u, v, w] <- (map read . words) <$> getLine
    return $ Map.insertWith (++) w [(u, v)] edges

perform roots counts (u, v) = do
    a' <- findRoot u
    b' <- findRoot v
    if a' == b'
       then return 0
       else do
           writeArray roots a' b'
           c1 <- readArray counts a'
           c2 <- readArray counts b'
           writeArray counts b' (c1+c2)
           return $ c1 * c2
  where
    findRoot x = do
        x' <- readArray roots x
        if x == x'
           then return x
           else writeArray roots x x' >> findRoot x'

