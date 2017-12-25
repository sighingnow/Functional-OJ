-- Codeforces 7C

{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Data.Int

main :: IO ()
main = do
    [a, b, c] <- (map read . words) <$> getLine
    let !(d, x, y) = exgcd a b
        !t = c `quot` d
    if c `rem` d /= 0
        then print (-1)
        else putStrLn . unwords . map show $ [- x * t, - y * t]

exgcd :: Integer -> Integer -> (Integer, Integer, Integer)
exgcd !a !0 = (a, 1, 0)
exgcd !a !b = let !(d, x, y) = exgcd b (a `rem` b)
             in (d, y, x - y * (a `quot` b))
