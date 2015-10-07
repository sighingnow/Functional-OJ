-- hackrank challenge - algorithm - 2-4: find digits

import Data.List

main :: IO ()
main = getLine >> getContents >>= putStrLn . intercalate "\n" . map show . solve . map read . words

solve :: [Integer] -> [Int]
solve = map (\n -> length $ filter (\k -> n `mod` k == 0) $ map (read . (\x -> [x])) $ filter (/= '0') (show n))

