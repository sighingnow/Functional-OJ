import           Data.List

main :: IO ()
main = do
    [n, d] <- (map read . words) <$> getLine
    ns <- (map read . words) <$> getLine :: IO [Int]
    let r = drop d ns ++ take d ns
    putStrLn $ unwords (map show r)
