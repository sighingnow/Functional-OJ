{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L

import Debug.Trace

main :: IO ()
main = do
    _ <- getLine
    xs <- (map (fst . fromJust . L.readInt) . L.split ' ') <$> L.getContents
    print $ solve 0 xs

part :: [Int] -> ([Int], [Int])
part ![] = ([], [])
part ![x] = ([], [x])
part (x1:x2:xs) = let !(ls, rs) = part (x2:xs)
                   in trace (show (ls, rs)) $ if x1 < x2
                         then (x2:ls, x1:rs)
                         else (ls, x1:x2:rs)

solve :: Int -> [Int] -> Int
solve d [] = d
solve d [x] = d
solve d ns =
    let (ls, rs) = part ns
     in if null ls
           then d
           else if null rs
               then d + 1
               else solve (d + 1) rs

