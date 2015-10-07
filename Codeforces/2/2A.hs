-- Codeforce 2A

import Data.Maybe
import qualified Data.Map as Map

main :: IO ()
main = do
    getLine
    getContents >>= putStrLn . solve . map ((\[name, score] -> (name, (read score) :: Int)) . words) . lines

solve :: [(String, Int)] -> String
solve xs = winner xs $ allwinners xs

winner :: [(String, Int)] -> Map.Map String Int -> String
winner ((name, score):xs) all
    | target == Nothing = winner xs all
    | score >= fromJust target = name
    | otherwise = winner xs (Map.insertWith (+) name (-score) all)
    where target = Map.lookup name all

allwinners :: [(String, Int)] -> Map.Map String Int
allwinners xs = Map.filter (== val) m where
    m = Map.fromListWith (+) xs
    val = maximum $ Map.elems m
