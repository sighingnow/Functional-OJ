-- Codeforces 630C

-- r = 2^1 + 2^2 + ... + 2^n = 2^{n+1} - 2

import Control.Applicative

main :: IO ()
main = do
    n <- read <$> getLine :: IO Integer
    print (2 ^ (n + 1) - 2)
