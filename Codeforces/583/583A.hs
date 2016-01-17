-- Codeforces 583A

import           Control.Monad
import           Data.List     (intercalate)
import qualified Data.Set      as Set

main :: IO ()
main = getLine >> getContents >>= putStrLn . intercalate " " . map show . solve 1 (Set.empty, Set.empty) . map read . words

solve :: Int -> (Set.Set Int, Set.Set Int) -> [Int] -> [Int]
solve _ _ [] = []
solve d (h, v) (i:j:xs)
    | (Set.member i h) || (Set.member j v) = solve (d+1) (h, v) xs
    | otherwise = d : solve (d+1) (Set.insert i h, Set.insert j v) xs
