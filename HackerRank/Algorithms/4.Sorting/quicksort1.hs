-- HackerRank challenge - Algorithms - 4-6: quicksort1

main :: IO ()
main = do
    _ <- getLine
    xs <- fmap (map read . words) getContents :: IO [Int]
    case xs of
         [] -> return ()
         x:_ -> putStr . unwords . map show . concatMap (flip filter xs) $ [(< x), (== x), (> x)]
