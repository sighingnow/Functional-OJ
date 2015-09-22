-- Codeforces 236A

import Data.List

main :: IO ()
main = getLine >>= putStrLn . solve . length . group . sort where
    solve num = if odd num then "IGNORE HIM!" else "CHAT WITH HER!"
