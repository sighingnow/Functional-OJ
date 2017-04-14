import           Control.Monad
import           Data.Array.IO
import           Data.List

main :: IO ()
main = do
    n <- read <$> getLine
    mat <- newArray (1, 2 * n) [] :: IO (IOArray Int [Int])
    replicateM_ n $ do
        [a, b] <- (map read . words) <$> getLine
        a' <- readArray mat a
        b' <- readArray mat b
        writeArray mat a (b:a')
        writeArray mat b (a:b')

    colored <- newArray (1, 2 * n) 0 :: IO (IOUArray Int Int)

    mapM_ (\i -> loop mat colored i i) [1..n]

    -- | m1: min, m2: max
    --
    -- usage of sort: could be optimized.
    counts <- (filter (> 1) . sort . map length . group . sort . filter (/= 0)) <$> getElems colored
    putStr . show . head $ counts
    putStr " "
    putStr . show . last $ counts

  where

    -- c: color, s: start, k: current, m: count of current color
    loop :: IOArray Int [Int] -> IOUArray Int Int -> Int -> Int -> IO ()
    loop mat colored c k = do
        x <- readArray colored k
        when (x == 0) $ do
            writeArray colored k c
            ns <- readArray mat k
            when (not . null $ ns) $ do
                mapM_ (loop mat colored c) ns
