{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE Strict #-}

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L

primes :: [Int]
primes = 2 :
    [ x
    | x <- [3 ..]
    , prime x ]
  where
    factors x = takeWhile (\p -> p * p <= x) primes
    prime x = all (\p -> x `mod` p > 0) (factors x)

main :: IO ()
main = do
    [_, q] <- (map read . words) <$> getLine :: IO [Int]
    xs <- (map (fst . fromJust . L.readInt) . L.split ' ') <$> L.getContents :: IO [Int]
    rs <- foldM (\s i -> do
        let (ls, rs) = partition (\x -> x `mod` (primes !! i) == 0) s
        forM_ ls print
        return (reverse rs)) xs [0..q-1]
    forM_ (reverse rs) print
