module Roman where

import Data.Maybe

solution :: String -> Int
solution [] = 0
solution [x] = fromJust (lookup x table)
solution (a:b:xs)
  | a' < b'   = (b'-a') + solution xs
  | otherwise = a' + solution (b:xs)
  where
    a' = fromJust (lookup a table)
    b' = fromJust (lookup b table)

table :: [(Char, Int)]
table = [
    ('I', 1),
    ('V', 5),
    ('X', 10),
    ('L', 50),
    ('C', 100),
    ('D', 500),
    ('M', 1000)
  ]
