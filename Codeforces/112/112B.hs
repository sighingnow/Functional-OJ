-- Codeforces 112B

-- If we want to divide a square into two equal parts, then the cutting line should 
-- pass through the center of our square. 

main :: IO ()
main = getLine >>= putStrLn . solve . map read . words where
    solve [n, x, y] = if (x==k||x==k+1) && (y==k||y==k+1) then "NO" else "YES" where k = n `div` 2
