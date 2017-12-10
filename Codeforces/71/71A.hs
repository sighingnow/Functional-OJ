-- Codeforces 71A

main :: IO()
main = interact $ unlines . map solve . tail . lines

solve :: [Char] -> [Char]
solve x
    | length x < 11 = x
    | otherwise = head x : show (length x-2) ++ [last x]

-- use `show` to do type cast.

