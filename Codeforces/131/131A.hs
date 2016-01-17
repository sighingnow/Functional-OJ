-- Codeforces 131A

import           Data.Char

main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve xs = if caps xs
    then xs
    else change xs where
        caps = foldl (\r c -> r || (isLower c)) False . tail
        change = map (\c -> if (isLower c) then (toUpper c) else (toLower c))
