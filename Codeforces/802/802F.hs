-- Codeforces 802F

-- Approximate the Poisson distribution by a normal one, the largest
-- value lies inside 2 sigma is about (95.45%)^250, near to zero.

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

readInts :: IO [Double]
readInts = map (fromIntegral . fst . fromJust . BC.readInt) . BC.words <$> BC.getLine

main :: IO ()
main = do
    t <- read <$> getLine :: IO Int
    forM_ [1..t] $ \_ -> do
        xs <- readInts
        putStrLn $ go xs
  where
    go xs = let mx = maximum $ map abs xs
                sigma = sqrt $ sum (map (^ (2 :: Int)) xs) / genericLength xs
             in if mx > 2 * sigma
                   then "poisson"
                   else "uniform"
