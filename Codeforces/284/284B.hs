-- Codeforces 284B

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    getLine >>= print . solve

solve :: String -> Int
solve xs = case cnt_I
    of {
        0 -> cnt_A;
        1 -> 1;
        otherwise -> 0;
    }where
        cnt_I = length $ filter (== 'I') xs
        cnt_A = length $ filter (== 'A') xs
