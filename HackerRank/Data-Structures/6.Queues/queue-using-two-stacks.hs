import Control.Monad
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed

main :: IO ()
main = do
    arr <- newArray_ (0, 100000) :: IO (IOUArray Int Int)
    q <- read <$> getLine
    foldM_ (\(a, b) _ -> do
        (x:xs) <- (map read . words) <$> getLine
        case x of
          1 -> writeArray arr b (head xs) >> return (a, b+1)
          2 -> return (a+1, b)
          3 -> readArray arr a >>= print >> return (a, b)) (0, 0) [1..q]

