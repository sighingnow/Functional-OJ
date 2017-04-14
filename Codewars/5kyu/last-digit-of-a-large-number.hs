module LastDigit where

-- forall integer, the last digit of its power takes 4 as cycle.
lastDigit :: Integer -> Integer -> Integer
lastDigit a b = (a `rem` 10) ^ (1 + (b-1) `rem` 4) `rem` 10
