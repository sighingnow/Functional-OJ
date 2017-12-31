-- Codeforces 450A

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# OPTIONS_GHC -optc-ffast-math #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Data.Int
import Data.List
import Data.Maybe

main :: IO ()
main = do
    (n:m:xs) <- (map (fromIntegral . fst . fromJust . BC.readInt) . BC.words) <$> BC.getContents :: IO [Double]
    print . snd . maximum $ zip (map (\x -> ceiling (x / m)) xs) [1..]
