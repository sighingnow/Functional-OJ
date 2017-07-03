import Control.Monad

main :: IO ()
main = do
    n <- read <$> getLine
    foldM_ (\st _ -> perform st) ([], []) [1..n]

perform :: ([Int], [Int]) -> IO ([Int], [Int])
perform (xs, ts) = do
    (n:ns) <- (map read . words) <$> getLine
    case n of
      1 -> return (head ns : xs, max1 (head ns) ts : ts)
      2 -> return (tail xs, tail ts)
      3 -> print (head ts) >> return (xs, ts)
  where
    max1 t ns = if null ns then t else max t (head ns)

