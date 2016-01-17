-- Codeforces 118A

import           Data.Char

main :: IO()
main = getLine >>= putStrLn . concat . map solve

solve :: Char -> [Char]
solve c
    | elem c "aeiouyAEIOUY" = ""
    | isLower c = ['.', c]
    | isUpper c = ['.', toLower c]

