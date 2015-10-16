{-# OPTIONS_GHC -O3 #-}

-- Codeforces 3B

import Data.Maybe
import Data.List
import Data.Function
import qualified Data.ByteString.Char8 as BC

import Debug.Trace

-- simple backpack, greedy algorithm

main :: IO ()
main = do
    [n, v] <- fmap (map read . words) getLine :: IO [Int]
    input <- fmap (sortBy (flip (compare `on` snd)) . zip [1..] . map (map (fst . fromJust . BC.readInt) . BC.words) . BC.lines) BC.getContents
    let a = map (\(idx, [t, v]) -> (idx, v)) $ filter (\x -> (snd x)!!0 == 1) input
    let b = map (\(idx, [t, v]) -> (idx, v)) $ filter (\x -> (snd x)!!0 == 2) input
    let (s, (ca, cb)) = solve n v a b
    print s
    putStrLn $ intercalate " " $ map show $ (map fst $ take ca a) ++ (map fst $ take cb b)

solve :: Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> (Int, (Int, Int))
solve n v a b = foldl' (\(s, (ca, cb)) i -> let t = ka!!(min la (v-2*i)) + kb!!i in if s > t then (s, (ca, cb)) else (t, (min la (v-2*i), i))) (0, (0, 0)) [0..(min lb (v `div` 2))]
    where
        (la, lb) = (length a, length b)
        [ka, kb] = map (scanl (\s e -> s + (snd e)) 0) [a, b]
