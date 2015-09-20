-- Codeforces 112A

import Data.Char
import Control.Applicative

main :: IO ()
main = do
    (a, b) <- liftA2 (,) (fmap (map toLower) getLine) (fmap (map toLower) getLine)
    print $ solve (a, b) where
        solve (a, b)
            | a < b = -1
            | a == b = 0
            | a > b = 1
