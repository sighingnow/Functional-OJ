-- Codeforces 451B

import           Data.List

main :: IO ()
main = getContents >>= putStrLn . solve . map read . words

solve :: [Int] -> String
solve (n:xs)
    | xs == (sort xs) = "yes\n1 1"
    | otherwise = case (reverse $ map fst k) == (map snd k)
    of {
        True -> "yes\n" ++ (intercalate " " $ map show [len+1, len+(length k)]);
        False -> "no";
    } where
        all = zip xs (sort xs)
        len = length $ takeWhile (\x -> (fst x) == (snd x)) all
        k = dropWhile (\x -> (fst x) == (snd x)) $ dropWhileEnd (\x -> (fst x) == (snd x)) all


-- judge if a list is sorted in non-decreasing ordering.
sorted :: [Int] -> Bool
sorted xs = and $ zipWith (<=) xs (tail xs)
