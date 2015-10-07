-- Codeforces 158C.hs

import Control.Applicative
import Control.Monad
import Data.List (intercalate)

main :: IO ()
main = do
    n <- fmap read getLine
    solve n

solve :: Int -> IO ()
solve n = foldM_ (\d _ -> command d) [] [1..n] where
    
    changepath :: [String] -> [String] -> [String]
    changepath base [] = base
    changepath base ("..":xs) = changepath (init base) xs
    changepath base (x:xs) = changepath (base++[x]) xs

    splitAll :: Char -> String -> [String]
    splitAll c [] = []
    splitAll c s = [takeWhile (/= c) s] ++ (splitAll c $ if null k then [] else tail k ) where k = dropWhile (/= c) s

    command current = do
        input <- getLine
        case (words input) of
            ["cd", path] -> return $ cd path where
                cd ('/':xs) = changepath [] (filter (not . null) (splitAll '/' xs))
                cd xs = changepath current (filter (not . null) (splitAll '/' xs))
            ["pwd"] -> do
                putStrLn $ intercalate "/" ("":current) ++ "/"
                return current
