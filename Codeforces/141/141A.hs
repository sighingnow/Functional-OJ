-- Codeforces 141A

import Control.Monad
import Data.List

main :: IO ()
main = do
    [a, b, c] <- replicateM 3 getLine
    putStrLn $ if (sort c) == (sort (a++b)) then "YES" else "NO"
