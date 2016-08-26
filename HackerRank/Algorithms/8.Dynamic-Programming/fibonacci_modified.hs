-- HackerRank challenge - Algorithms - 8-1: fibonacci-modified

main :: IO ()
main = fmap words getLine >>= print . algo . map (read :: String -> Integer)
  where
    algo [ t1, t2, n ] = go 2 t1 t2
      where
        go k x1 x2
            | k == n = x2
            | otherwise = go (k + 1) x2 (x1 + x2 * x2)
