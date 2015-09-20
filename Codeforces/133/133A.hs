-- Codeforces 133A.hs

import Data.List

main :: IO ()
main = getLine >>= putStrLn . solve where
    solve :: String -> String
    solve s = if (valid s) then "YES" else "NO" where
        valid :: String -> Bool
        valid s = ('H' `elem` s) || ('Q' `elem` s) || ('9' `elem` s)
