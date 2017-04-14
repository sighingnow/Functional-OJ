-- HackerRank challenge - Algorithms - 8-7: even-tree

import           Data.HashMap.Strict ( HashMap (..) )
import qualified Data.HashMap.Strict as M
import           Data.HashTable.Class
import           Data.Array
import           Data.Array.IO
import           Data.Array.MArray

type Map = HashMap

main :: IO ()
main = do
    [n, m] <- fmap (map read . words) getLine :: IO [Int]
    graph <- fmap (buildMap M.empty . map read . words) getContents :: IO (Map Int [Int])
    r <- newArray (1, n) False :: IO (IOArray Int Bool)
    arr <- newArray (1, n) 0 :: IO (IOArray Int Int)
    _ <- compute graph arr r 1
    getElems arr >>= print . (\x -> length x - 1) . filter even
  where
    buildMap m [] = m
    buildMap m (a:b:xs) = buildMap (M.insertWith (++) b [a] (M.insertWith (++) a [b] m)) xs
    compute :: Map Int [Int] -> IOArray Int Int -> IOArray Int Bool -> Int -> IO Int
    compute g arr r k = do
        e <- readArray r k
        if e then return 0
             else do
                 writeArray r k True
                 rs <- fmap ((1+) . sum) $ mapM (\x -> do
                                                 e' <- readArray r x
                                                 if e' then return 0
                                                       else compute g arr r x) (g M.! k)
                 writeArray arr k rs
                 return rs
