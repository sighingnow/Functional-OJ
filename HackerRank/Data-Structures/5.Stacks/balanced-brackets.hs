import Control.Monad

main :: IO ()
main = do
    n <- read <$> getLine
    replicateM_ n (getLine >>= putStrLn . perform [])

perform :: String -> String -> String
perform [] [] = "YES"
perform _ [] = "NO"
perform [] (x:xs) = perform [x] xs
perform (s:ss) (x:xs) = if match s x then perform ss xs else perform (x:s:ss) xs

match :: Char -> Char -> Bool
match '(' ')' = True
match '[' ']' = True
match '{' '}' = True
match _ _ = False


