-- Codeforces 465A

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    getLine >>= print . solve

solve :: String -> Int
solve xs = length $ filter (\i -> (xs!!i) /= (ys!!i)) [0..(length xs)-1] where
    ys = fst $ foldl (\(acc, carry) b ->
        case (carry, b) of
            ('1', '1') -> (acc++['0'], '1')
            ('1', '0') -> (acc++['1'], '0')
            ('0', '1') -> (acc++['1'], '0')
            ('0', '0') -> (acc++['0'], '0'))
        ([], '1') xs
