-- Codeforces 534A

import Data.List
import Data.Maybe

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    let result = solve n
    putStrLn $ (show $ length result) ++ "\n" ++ concatMap (\x -> show x ++ " ") result

solve :: Int -> [Int]
solve 1 = [1]
solve 2 = [1]
solve 3 = [1, 3]
solve n = (reverse $ filter odd [1..n]) ++ (reverse $ filter even [1..n])
