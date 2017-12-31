-- Codeforces 911A

{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -optc-O0 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Maybe

main :: IO ()
main = do
    (n:ns) <- (map (fst . fromJust . BC.readInt) . BC.words) <$> BC.getContents :: IO [Int]
    let a = minimum ns
        indices = elemIndices a ns
        diff = zipWith (-) (tail indices) indices
        ans = minimum diff
    print ans
