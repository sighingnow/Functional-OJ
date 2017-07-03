module Base (rebase) where

import Data.List

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits = case get of
                                            Nothing -> Nothing
                                            Just value -> Just $ put value
  where
    get = if inputBase <= 1 || outputBase <= 1
                            || any (\x -> x < 0 || x >= inputBase) inputDigits
            then Nothing
            else Just $ foldl (\acc x -> acc * inputBase + x) 0 inputDigits

    swap (a, b) = (b, a)

    put = reverse . unfoldr (\v -> if v == 0 then Nothing else Just $ swap $ divMod v outputBase)
