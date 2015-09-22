-- Codeforces 58A

import Data.List

main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve xs = if (length $ snd $ foldl (\(s, acc) c -> let k = dropWhile (/= c) s in if null k then (s, acc) else (tail k, acc ++ [head k])) (xs, "") "hello") == 5 then "YES" else "NO"
