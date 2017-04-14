module LastDigit (lastDigit) where

-- forall integer, the last digit of its power takes 4 as cycle.
lastDigit :: [Integer] -> Integer
lastDigit [] = 1
lastDigit (x:xs) = ((x `rem` 10) ^ r) `rem` 10
    where
        r = case xs of
                 [] -> 1
                 0:as -> if isZero as then 1 else 0
                 a:as -> case a `rem` 4 of
                              0 | isZero as -> 1
                                | otherwise -> 4
                              1 -> 1
                              2 | isZero as -> 1
                                | isOne as -> 2
                                | otherwise -> 4
                              _ | isOdd as -> 3
                                | otherwise -> 1
        isZero [] = False
        isZero (0:vs) = not (isZero vs)
        isZero (_:_) = False
        isOne [] = True
        isOne (1:_) = True
        isOne (_:vs) = isZero vs
        isOdd [] = True
        isOdd (v:vs) = odd v || isZero vs
