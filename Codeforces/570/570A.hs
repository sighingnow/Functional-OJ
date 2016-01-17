-- Codeforces 570A

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe

main :: IO ()
main = do
    [n, m] <- fmap (map read . words) getLine :: IO [Int]
    replicateM m getLine >>= print . solve . map (map read . words)

solve :: [[Int]] -> Int
solve = snd . head . sortBy (\x y -> if (fst x) == (fst y) then compare (snd x) (snd y) else compare (fst y) (fst x)) . map (\x -> (length x, head x)) . group . sort . map (\x -> (+ 1) $ fromJust $ elemIndex (maximum x) x)
