-- Codeforces 148A

import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = replicateM 5 getLine >>= print . solve . map read where
    solve xs = (last xs) - (length $ foldl (\all c -> filter (\x -> x `mod` c /= 0) all) [1..(last xs)] (init xs))

    
