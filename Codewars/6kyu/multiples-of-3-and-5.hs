module MultiplesOf3And5 where

solution :: Integer -> Integer
solution number = sum $ filter (\n -> rem n 3 == 0 || rem n 5 == 0) [0..number-1]
