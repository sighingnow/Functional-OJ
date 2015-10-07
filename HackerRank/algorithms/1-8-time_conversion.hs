-- Hackrank challenge - algorithm - 1-8: time conversion

import Data.List
import Text.Printf

main :: IO ()
main = do
    [h, m, s] <- fmap (\s -> solve (read $ slice s (1,2)) (read $ slice s (4,5)) (read $ slice s (7,8)) (s!!8)) getLine
    printf "%02d:%02d:%02d" h m s

slice :: String -> (Int, Int) -> String
slice s (l,r) = drop (l-1) $ take r s

solve :: Int -> Int -> Int -> Char -> [Int]
solve h m s flag
    | flag == 'A' = [h `mod` 12, m, s]
    | flag == 'P' = [h `mod` 12 + 12, m, s]
