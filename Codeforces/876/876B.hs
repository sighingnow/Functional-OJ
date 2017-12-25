-- Codeforces 876B

-- Seems that the answer is order-sensitive, I haven't make this Haskell solution be accepted.

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -funfolding-use-threshold=16 #-}
{-# OPTIONS_GHC -fexcess-precision #-}
{-# OPTIONS_GHC -optc-O3 #-}
{-# OPTIONS_GHC -optc-ffast-math #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import Data.Function
import Data.List
import Data.Maybe

main :: IO ()
main = do
    [n, k, m] <- (map read . words) <$> getLine
    xs <- (map (fst . fromJust . BC.readInt) . BC.words) <$> BC.getContents
    let !(z:zs) = sortBy (compare `on` fst) $ map (`mod` m) xs `zip` xs
    case go z zs 0 1 k of
        Nothing -> putStrLn "No"
        Just idx -> do
            print (z:zs)
            print idx
            putStrLn "Yes"
            let !rs = take k $ drop idx (z:zs)
            putStrLn $ intercalate " " (map (show . snd) rs)

  where
    go !z [] !idx !i !k
      | i == k = Just $ idx - k
      | otherwise = Nothing
    go !z !(x:xs) !idx !i !k
      | i == k = Just $ idx - k
      | fst z == fst x = go x xs (idx + 1) (i + 1) k
      | otherwise = go x xs (idx + 1) 1 k
