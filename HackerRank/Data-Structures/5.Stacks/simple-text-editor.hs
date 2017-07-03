{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

import Control.Monad
import Data.Array.Unboxed
import Data.Array.IO
import Data.Array.MArray
import Data.List

data Op = Append !String
        | Delete {-# UNPACK #-}!Int
        | Print {-# UNPACK #-}!Int
        | Undo

readOp :: IO Op
readOp = do
    (k:s) <- words <$> getLine
    return $
        case (read k :: Int) of
          1 -> Append (s !! 0)
          2 -> Delete (read (s !! 0))
          3 -> Print (read (s !! 0))
          _ -> Undo

{-# INLINE readOp #-}

main :: IO ()
main = do
    q <- read <$> getLine :: IO Int
    arr <- newArray_ (1, 1000001) :: IO (IOUArray Int Char)
    foldM_ (\(idx, ops) _ -> do
        op <- readOp
        perform idx arr op ops False) (1, []) [1..q]

sliceArr :: IOUArray Int Char -> Int -> Int -> IO [Char]
sliceArr arr a b
    | a < b = do
        c <- readArray arr a
        (c:) <$> sliceArr arr (a+1) b
    | otherwise = return []

{-# INLINE sliceArr #-}

appendArr :: Int -> IOUArray Int Char -> [Char] -> IO Int
appendArr idx arr [] = return idx
appendArr idx arr (c:s) = writeArray arr idx c >> appendArr (idx + 1) arr s

{-# INLINE appendArr #-}

perform :: Int -> IOUArray Int Char -> Op -> [Op] -> Bool -> IO (Int, [Op])
perform idx arr (Append s) ops undo = do
    idx' <- appendArr idx arr s
    return $ if undo
                then (idx', ops)
                else (idx', (Delete (idx' - idx)) : ops)
perform idx arr (Delete k) ops undo = do
    s <- sliceArr arr (idx - k) idx
    return $ if undo
                then (idx - k, ops)
                else (idx - k, (Append s) : ops)
perform idx arr (Print k) ops _ = do
    readArray arr k >>= putChar >> putChar '\n'
    return (idx, ops)
perform idx arr Undo (op:ops) _ = do
    perform idx arr op ops True

