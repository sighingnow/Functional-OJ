{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 -optc-O2 #-}

-- Codeforces 547B

import Control.Monad
import Control.Monad.Primitive
import Data.Maybe
import Data.IORef
import qualified Data.List as L
import qualified Data.ByteString.Char8 as BC

import Data.Vector.Fusion.Stream.Monadic

import qualified Data.Vector.Unboxed.Mutable as Vec

-- history:
-- 1. 2015-09-23: finish this solution, but time limit exceeded.
-- 2. 2015-09-26: re-write this solution using `Data.Vertor`, but compilation error, said...

-- For each i, find the largest j that aj < ai and show it by li (if there is no such j, then li = 0).
-- Also, find the smallest j that aj < ai and show it by ri (if there is no such j, then ri = n + 1).
-- This can be done in O(n) with a stack.

-- For each i, we know that ai can be minimum element in groups of size 1, 2, ..., ri - li - 1.

-- reference solution (by PrinceOfPersia): http://ideone.com/Ryg2gn

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    BC.getContents >>= solve n . L.map (fst . fromJust . BC.readInt) . BC.words >>= BC.putStrLn

solve :: Int -> [Int] -> IO BC.ByteString
solve n xs = do
    stk <- Vec.new (n+5)
    lbv <- Vec.new (n+5)
    rbv <- Vec.new (n+5)
    ans <- Vec.new (n+5)
    
    Vec.write stk 0 0       -- reset stk.
    Control.Monad.forM_ (L.zip [0..] xs) (\k -> getbound xs (stk, lbv) k (-1))
    Vec.write stk 0 0       -- reset stk.
    Control.Monad.forM_ (L.reverse (L.zip [0..] xs)) (\k -> getbound xs (stk, rbv) k (n))
    
    Control.Monad.forM_ (L.zip [0..] xs) (\k -> initial k lbv rbv ans)
    Control.Monad.forM_ (reverse [0..(n-1)]) (\k -> update k ans)
    
    fmap (BC.intercalate (BC.singleton ' ')) $ fmap (L.map (BC.pack . show)) $ Control.Monad.mapM (Vec.read ans) [1..n] >>= return

initial :: (PrimMonad m) => (Int, Int) -> Vec.MVector (PrimState m) Int -> Vec.MVector (PrimState m) Int -> Vec.MVector (PrimState m) Int -> m ()
initial (i, x) lbv rbv ans = do
    l <- Vec.read lbv i
    r <- Vec.read rbv i
    old <- Vec.read ans (r-l-1)
    Vec.write ans (r-l-1) (max old x)

update :: (PrimMonad m) => Int -> Vec.MVector (PrimState m) Int -> m ()
update loc ans = do
    a <- Vec.read ans loc
    b <- Vec.read ans (loc+1)
    Vec.write ans loc (max a b)

getbound :: (PrimMonad m) => [Int] -> (Vec.MVector (PrimState m) Int, Vec.MVector (PrimState m) Int) -> (Int, Int) -> Int -> m ()
getbound xs (stk, bound) (i, x) dv = do
    popgreater xs stk x
    top <- Vec.read stk 0
    if top >= 1
        then do                                 -- not empty.
            val <- Vec.read stk top
            Vec.write bound i val
        else do                                 -- empty.
            Vec.write bound i dv                -- set bound to default value. (`-1` for left bound and `n` for left bound)
    push stk i

popgreater :: (PrimMonad m) => [Int] -> Vec.MVector (PrimState m) Int -> Int -> m ()
popgreater xs stk val = do
    top <- Vec.read stk 0                       -- get stack top.
    if top >= 1
        then do                                 -- not empty.
            loc <- Vec.read stk top
            if (xs L.!! loc) >= val
                then do
                    Vec.write stk 0 (top-1)     -- update stk top.
                    popgreater xs stk val          -- continue pop.
                else do                         -- stop pop.
                    return ()
        else do                                 -- empty.
            return ()

push :: (PrimMonad m) => Vec.MVector (PrimState m) Int -> Int -> m ()
push stk val = do
    top <- Vec.read stk 0       -- get stack top.
    Vec.write stk 0 (top+1)     -- update top.
    Vec.write stk (top+1) val   -- push val to stk.

-- old solution: Time limit exceeded.
-- 
--solve1 :: Int -> [Int] -> BC.ByteString
--solve1 n xs = BC.intercalate (BC.pack " ") $ map (BC.pack . show) $ init $ scanr (\x acc -> max x acc) (head k) (tail k) where
--    k = map snd $ M.toList $ foldl' (\ans i -> M.insert ((rb!!i)-(lb!!i)-1) (max (ans M.! ((rb!!i)-(lb!!i)-1)) (xs!!i)) ans) (foldl' (\m i -> M.insert i 0 m) M.empty [0..n]) [0..n-1] where
--        lb = map snd $ sort $ snd $ foldl' (\(stk, l) (i, x) -> let s = dropWhile (\t -> xs!!t >= xs!!i) stk in (i:s, (i, if null s then (-1) else head s):l)) ([], []) $ zip [0..] xs
--        rb = map snd $ sort $ snd $ foldl' (\(stk, l) (i, x) -> let s = dropWhile (\t -> xs!!t >= xs!!i) stk in (i:s, (i, if null s then n else head s):l)) ([], []) $ reverse $ zip [0..] xs
