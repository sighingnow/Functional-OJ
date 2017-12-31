-- Codeforces 797B

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
    getLine
    xs <- (map (fst . fromJust . BC.readInt) . BC.words) <$> BC.getContents
    let (s, k) = foldl' (\(s, k) x -> if x >= 0
                                         then if x `mod` 2 == 1
                                                 then (s + x, min k x)
                                                 else (s + x, k)
                                         else if x `mod` 2 == 1
                                                 then (s, min k (- x))
                                                 else (s, k)) (0, maxBound :: Int) xs
    if s `mod` 2 == 0
       then print (s - k)
       else print s
