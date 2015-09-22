-- Codeforces 572B

import Control.Applicative
import Control.Monad
import Data.List
import Data.Char

main :: IO ()
main = do
    [n, s] <- fmap (map read . words) getLine :: IO [Int]
    replicateM n getLine >>= putStrLn . solve s . map words

solve :: Int -> [[String]] -> String
solve s book = foldl (\acc a -> (acc++a++"\n")) "" $ (reverse $ take s $ reverse slist) ++ (take s blist) where
    [slist, blist] = map (\c -> map (\ss -> c ++ " " ++ (ss!!0!!1) ++ " " ++ (show (foldl (\acc k -> acc + (read (k!!2) :: Int)) 0 ss))) $ groupBy (\a b -> (a!!1)==(b!!1)) $ sortBy (\a b -> compare (read (b!!1) :: Int) (read (a!!1) :: Int)) $ filter (\x -> (x!!0) == c) book) ["S", "B"]
