{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- Codeforces 404B

import           Control.Applicative
import           Data.Fixed          (mod')
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

main :: IO ()
main = do
    [a, d] <- fmap (map read . words) getLine
    n <- fmap read getLine
    newCString "%.8lf %.8lf\n" >>= solve a d n

foreign import ccall "stdio.h printf"
    c_printf :: CString -> Double -> Double -> IO CInt

solve :: Double -> Double -> Int -> CString -> IO ()
solve a d n format = mapM_ (ps . handle . (*) d . fromIntegral) [1..n] where
    handle p = case p `mod'` (4*a) of
        q | q < a -> (q, 0)
        q | q < 2 * a -> (a, q-a)
        q | q < 3 * a -> (3*a-q, a)
        q | otherwise -> (0, 4*a-q)
    ps (a, b) = c_printf format a b where
