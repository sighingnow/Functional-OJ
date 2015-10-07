-- Codeforces 257B

-- Game theory

main :: IO ()
main = getLine >>= putStrLn . (\(cn, cm) -> show cn ++ " " ++ show cm) . (\[n, m] -> (m+n-(min m n)-1, min n m)) . map read . words
