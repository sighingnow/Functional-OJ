-- Codeforces 556A

-- Hint: If there still exist at least one 0 and at least one 1 in the string 
-- then there obviously exists either substring 01 or substring 10 (or both) 
-- and we can remove it. 
-- The order in which we remove substrings is unimportant: in any case we will 
-- make min(#zeros, #ones) such operations. 
-- Thus the answer is #ones + #zeros - 2min(#ones, #zeros) = |#ones - #zeros|.

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    getLine >>= print . abs . solve where
        solve xs = (length $ filter (== '1') xs) - (length $ filter (== '0') xs)
