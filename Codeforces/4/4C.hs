-- Codeforces 4C

import           Control.Applicative
import           Control.Monad
import           Data.List
import qualified Data.Map            as Map

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    solve Map.empty n

-- Note:
--  1. don't store the result, input a line, then compute, then print result.
--  2. use Map.insertWith to combine the old value and new value.

solve :: Map.Map String Int -> Int -> IO ()
solve _ 0 = return ()
solve db n = do
    req <- getLine
    putStrLn $ case Map.lookup req db of
        Just c -> req ++ show c
        Nothing -> "OK"
    solve (Map.insertWith (+) req 1 db) (n-1)
