-- Codeforces 420A

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -optc-O2 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Data.List

main :: IO ()
main = do
    xs <- getLine
    if xs == reverse xs && all (`elem` "AHIMTWYOXVU") xs
       then putStrLn "YES"
       else putStrLn "NO"
