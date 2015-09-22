-- Codefoces 281A

import Data.Char

main :: IO ()
main = do
    s <- getLine
    putStrLn $ [(toUpper $ head s)] ++ (tail s)
