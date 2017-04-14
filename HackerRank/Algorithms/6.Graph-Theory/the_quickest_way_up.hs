-- HackerRank challenge - Algorithms - 8-18: the-quickest-way-up

{-# LANGUAGE FlexibleContexts #-}

import           Control.Monad
import           Data.Array.IO
import           Data.Maybe

main :: IO ()
main = do
    t <- fmap read getLine :: IO Int
    replicateM t perform >>= putStrLn . unlines . map show

readEdge :: IOUArray Int Int -> IO ()
readEdge arr = do
    [u, v] <- fmap (map read . words) getLine
    writeArray arr u v

perform :: IO Int
perform = do
    arr <- newListArray (1, 100) [1..100] :: IO (IOUArray Int Int)
    vis <- newArray (1, 100) False :: IO (IOUArray Int Bool)
    dist <- newArray (1, 100) 0 :: IO (IOUArray Int Int)
    n <- fmap read getLine :: IO Int
    replicateM_ n (readEdge arr)
    m <- fmap read getLine :: IO Int
    replicateM_ m (readEdge arr)
    writeArray vis 1 True
    search arr dist vis [1]

search :: IOUArray Int Int -> IOUArray Int Int -> IOUArray Int Bool -> [Int] -> IO Int
search arr dist vis [] = do
    x <- readArray vis 100
    if x then readArray dist 100
         else return (-1)
search arr dist vis (100:xs) = readArray dist 100
search arr dist vis (x:xs) = do
    ns <- fmap catMaybes $ mapM (enqueue x) [1..6]
    search arr dist vis (xs ++ ns)
    where
        enqueue node i = do
            if (node+i) >= 1 && (node+i) <= 100
               then do
                   nxt <- readArray arr (node+i)
                   if nxt >= 1 && nxt <= 100
                      then do
                          v <- readArray vis nxt
                          if (not v)
                             then do
                                 writeArray vis nxt True
                                 d <- readArray dist node
                                 writeArray dist nxt (d + 1)
                                 return $ Just nxt
                             else return Nothing
                       else return Nothing
               else return Nothing
