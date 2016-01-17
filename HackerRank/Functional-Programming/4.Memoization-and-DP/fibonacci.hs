-- reference: https://wiki.haskell.org/The_Fibonacci_sequence

fibs :: [Int]
fibs = 0 : 1 : zipWith (\a b -> (a + b) `rem` 100000007) fibs (tail fibs)

fibs' :: [Int]
fibs' = 0 : scanl (\a b -> (a + b) `rem` 100000007) 1 fibs'

fib :: Int -> Int
fib n = fibs !! n

main :: IO ()
main = getContents >>= mapM_ (print . fib) . tail . map (read :: String -> Int) . words

