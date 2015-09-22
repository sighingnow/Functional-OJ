-- Codeforces 552B

main :: IO ()
main = getLine >>= print . solve . read

-- use interger.
solve :: Integer -> Integer
solve = count 1 1 10 where
    count len l r x = if (x < r)
        then (x-l+1) * len
        else (r-l) * len + count (len+1) (l*10) (r*10) x
