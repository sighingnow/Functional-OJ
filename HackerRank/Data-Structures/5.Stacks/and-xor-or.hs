{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE Strict #-}

import Data.Bits
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    _ <- getLine
    xs <- ( map (fst . fromJust . L.readInt) . L.split ' ') <$> L.getContents
    print $ perform [] 0 xs

perform :: [Int] -> Int -> [Int] -> Int
perform _ m [] = m
perform [] m (x:xs) = perform [x] m xs
perform st m (x:xs) = let (st', m') = popfrom st m x in perform (x:st') m' xs
  where
    popfrom :: [Int] -> Int -> Int -> ([Int], Int)
    popfrom [] m _ = ([], m)
    popfrom st@(s:ss) m x
        | x < s = popfrom ss m' x
        | otherwise = (st, m')
        where m' = max m (x `xor` s)

