module Reverse where
import Prelude hiding (reverse)

-- | Reverse a list
reverse :: [a] -> [a]
reverse xs = rev xs [] where
  rev [] xs     = xs
  rev (t:ts) xs = rev ts (t:xs)
