main :: IO ()
main = do
    _ <- getLine
    ns1 <- (scanr1 (+) . map read . words) <$> getLine
    ns2 <- (scanr1 (+) . map read . words) <$> getLine
    ns3 <- (scanr1 (+) . map read . words) <$> getLine
    print $ resolve ns1 ns2 ns3

resolve :: [Int] -> [Int] -> [Int] -> Int
resolve [] _ _ = 0
resolve _ [] _ = 0
resolve _ _ [] = 0
resolve s1@(n1:ns1) s2@(n2:ns2) s3@(n3:ns3)
  | n1 == n2 && n2 == n3 = n1
  | n1 >= n2 && n1 >= n3 = resolve ns1 s2 s3
  | n2 >= n1 && n2 >= n3 = resolve s1 ns2 s3
  | n3 >= n1 && n3 >= n2 = resolve s1 s2 ns3
  | otherwise = error "Impossible"

