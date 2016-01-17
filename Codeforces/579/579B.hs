-- Codeforces 579B

import           Control.Monad
import qualified Data.ByteString.Char8 as BC
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Set              as Set

-- Hint: get all combinations of (i, j), then sort it by the value of `d` in decreasing order.

-- Note: use Data.ByteString.Char8.ReadInt to accelerate IO action.

main :: IO ()
main = BC.getContents >>= putStrLn . intercalate " " . map show . solve . map (fst . fromJust . BC.readInt) . BC.words

solve :: [Int] -> [Int]
solve (n:xs) = map snd $ sort $ fst $ foldl (\(now, acc) (x, y) -> if (Set.member x acc) || (Set.member y acc) then (now, acc) else ((x,y):(y,x):now, (Set.insert x $ Set.insert y acc))) ([], Set.empty) $ map snd $ reverse $ sort $ zip xs [(i,j) | i <- [1..2*n], j <- [1..i-1]]
