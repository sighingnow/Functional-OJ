-- Hackrank challenge - algorithm - 1-6: plus minus

import Text.Printf

main :: IO ()
main = do
    n <- readLn :: IO Int
    numbers <- fmap (map (read :: String -> Int) . words) getLine
    printf "%.3f\n" (((fromIntegral :: Int -> Float) $ length $ filter (>0) numbers)/(fromIntegral n))
    printf "%.3f\n" (((fromIntegral :: Int -> Float) $ length $ filter (<0) numbers)/(fromIntegral n))
    printf "%.3f\n" (((fromIntegral :: Int -> Float) $ length $ filter (==0) numbers)/(fromIntegral n))
