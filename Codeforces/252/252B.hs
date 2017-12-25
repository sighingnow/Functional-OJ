-- Codeforces 252B

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -funfolding-use-threshold=16 #-}
{-# OPTIONS_GHC -fexcess-precision #-}
{-# OPTIONS_GHC -feager-blackholing #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fdicts-strict #-}
{-# OPTIONS_GHC -optc-O3 #-}
{-# OPTIONS_GHC -optc-ffast-math #-}

import Control.Applicative
import Data.Maybe
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    xs <- (map (fst . fromJust . BC.readInt) . BC.words) <$> BC.getContents :: IO [Int]
    putStrLn . unwords . map show . go . zip [1..] $ xs
  where
    go ((x1,v1):(x2,v2):(x3,v3):vs)
      | v1 == v2 && v2 == v3 = go $ (x2,v2):(x3,v3):vs
      | v1 == v2 = [x2, x3]
      | v2 == v3 = [x1, x2]
      | v1 > v2 && v2 > v3 = [x1, x2]
      | v1 < v2 && v2 < v3 = [x1, x2]
      | v1 /= v3 = [x1, x3]
      | null vs = [-1]
      | v1 == v4 = [x2, x3]
      | v2 == v4 = [x1, x2]
      | v3 == v4 = [x2, x3]
      | otherwise = [x3, x4]
      where (x4, v4) = head vs
    go _ = [-1]
