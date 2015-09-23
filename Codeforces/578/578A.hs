-- Codeforces 578A

main :: IO ()
main = getLine >>= print . solve . map read . words where
    solve :: (RealFrac a) => [a] -> a
    solve [x, y]
        | x < y = -1
        | otherwise = (x+y)/2/(fromIntegral ((fromIntegral $ floor (x+y)) `div` (fromIntegral $ floor (2*y))))
