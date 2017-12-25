-- Codeforces 547B

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# OPTIONS_GHC -optc-ffast-math #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST.Safe
import           Data.Array.Unboxed
import           Data.Array.ST.Safe
import qualified Data.ByteString.Char8 as BC
import           Data.Foldable
import           Data.Maybe
import           Data.STRef

-- For each i, find the largest j that aj < ai and show it by li (if there is no such j, then li = 0).
-- Also, find the smallest j that aj < ai and show it by ri (if there is no such j, then ri = n + 1).
-- This can be done in O(n) with a stack.

-- For each i, we know that ai can be minimum element in groups of size 1, 2, ..., ri - li - 1.

-- reference solution (by PrinceOfPersia): http://ideone.com/Ryg2gn

main :: IO ()
main = do
    n <- fmap read getLine
    BC.getContents >>= BC.putStrLn . BC.intercalate " " . map (BC.pack . show) . solve n . map (fst . fromJust . BC.readInt) . BC.words

popgreater :: Int -> STRef s Int -> STUArray s Int Int -> STUArray s Int Int -> ST s (Maybe Int)
popgreater k top stk xs = do
    vtop <- readSTRef top
    if vtop > 0
       then do
          idx <- readArray stk (vtop - 1)
          x <- readArray xs idx
          if x >= k
             then do
                writeSTRef top (vtop - 1)
                popgreater k top stk xs
             else return (Just idx)
       else return Nothing

pushback :: Int -> STRef s Int -> STUArray s Int Int -> ST s ()
pushback k top stk = do
    vtop <- readSTRef top
    writeArray stk vtop k
    writeSTRef top (vtop + 1)

solve :: Int -> [Int] -> [Int]
solve n xs = elems $ runSTUArray $ do
    xsarr <- newListArray (0, n - 1) xs :: ST s (STUArray s Int Int)
    ls <- newArray (0, n - 1) (-1) :: ST s (STUArray s Int Int)
    rs <- newArray (0, n - 1) n :: ST s (STUArray s Int Int)
    stk <- newArray_ (0, n - 1) :: ST s (STUArray s Int Int)

    top <- newSTRef 0 -- initialize stack.
    for_ [0 .. n-1] $ \i -> do
        v <- readArray xsarr i
        x <- popgreater v top stk xsarr
        case x of
            Nothing -> return ()
            Just idx -> writeArray ls i idx
        pushback i top stk

    writeSTRef top 0 -- clear stack.
    for_ [n-1, n-2 .. 0] $ \i -> do
        v <- readArray xsarr i
        x <- popgreater v top stk xsarr
        case x of
            Nothing -> return ()
            Just idx -> writeArray rs i idx
        pushback i top stk

    ans <- newArray (1, n) 0 :: ST s (STUArray s Int Int)

    for_ [0 .. n-1] $ \i -> do
        l <- readArray ls i
        r <- readArray rs i
        let k = r - l - 1
        x <- readArray ans k
        v <- readArray xsarr i
        writeArray ans k (max x v)

    for_ [n-1, n-2 .. 1] $ \i -> do
        a <- readArray ans i
        b <- readArray ans (i + 1)
        writeArray ans i (max a b)

    return ans
