-- Codeforces 246B

import qualified Data.ByteString.Char8 as BC
import Data.Maybe

main :: IO ()
main = BC.getContents >>= print . solve . map (fst . fromJust . BC.readInt) . BC.words

solve :: [Int] -> Int
solve (n:xs) = if (sum xs) `mod` n  == 0 then n else (n-1)
