module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
    | n > 3000 || n <= 0 = Nothing
    | otherwise = Just $ translate n
  where
    translate n
        | n >= 1000 = 
