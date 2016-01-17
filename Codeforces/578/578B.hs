-- Codeforces 578B

import           Data.Bits             ((.|.))
import qualified Data.ByteString.Char8 as BC
import           Data.Foldable         (foldl', foldr')
import           Data.Maybe            (fromJust)

-- Hint multiply x^k to a_i to get the hightest bit 1 to make result to be to greatest.

-- Note: use Data.ByteString.Char8.ReadInt to accelerate IO action.
--       Use scanl1 and scanr1 to record the intermediate list to reduce time and memory space usage.

main :: IO ()
main = BC.getContents >>= print . solve . map (toInteger . fst . fromJust . BC.readInt) . BC.words

solve :: [Integer] -> Integer
solve (n:k:x:xs) = maximum $ zipWith3 or xs prefix suffix where
    or a p s = a * b .|. p .|. s where b = x ^ k
    prefix = 0:scanl1 (.|.) xs
    suffix = tail $ scanr1 (.|.) xs ++ [0]
