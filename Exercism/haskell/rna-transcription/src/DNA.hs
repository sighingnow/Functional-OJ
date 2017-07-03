module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA [] = Just []
toRNA (x:xs) = do
    r <- case x of
           'G' -> Just 'C'
           'C' -> Just 'G'
           'T' -> Just 'A'
           'A' -> Just 'U'
           _   -> Nothing
    rs <- toRNA xs
    return (r:rs)
