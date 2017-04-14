-- HackerRank challenge - Algorithms - 1-3: compare the triplets

import           Control.Monad

main :: IO ()
main = do
    as <- (map read . words) <$> getLine :: IO [Int]
    bs <- (map read . words) <$> getLine :: IO [Int]
    let ns = zip as bs
    putStr . show $ sum $ map (\(a, b) -> if a > b then 1 else 0) ns
    putStr " "
    putStr . show $ sum $ map (\(a, b) -> if a < b then 1 else 0) ns
