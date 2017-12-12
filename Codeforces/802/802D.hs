-- Codeforces 802D

-- MLP

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -funfolding-use-threshold=16 #-}
{-# OPTIONS_GHC -fexcess-precision #-}
{-# OPTIONS_GHC -feager-blackholing #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fdicts-strict #-}
{-# OPTIONS_GHC -optc-O3 #-}
{-# OPTIONS_GHC -optc-ffast-math #-}

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BC

readInts :: IO [Int]
readInts = map (fst . fromJust . BC.readInt) . BC.words <$> BC.getLine

main :: IO ()
main = do
    t <- read <$> getLine :: IO Int
    forM_ [1..t] $ \_ -> do
        xs <- readInts
        putStrLn $ go xs
  where
    go :: [Int] -> String
    go xs = let p = fromIntegral (sum xs) / genericLength xs -- estimate p
                puniform = sum . map (uniform p) $ xs
                ppoisson = sum . map (poisson p) $ xs
             in if puniform > ppoisson
                   then "uniform"
                   else "poisson"

    uniform p _ = - log (2 * p + 1)
    poisson p k = fromIntegral k * log p - p - logfact !! k

    logfact :: [Double]
    logfact = scanl (\acc x -> acc + log x) 0 [1..]
