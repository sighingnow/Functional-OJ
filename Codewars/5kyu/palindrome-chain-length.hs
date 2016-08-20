module PalindromeChain where

palindromeChainLength :: Integer -> Integer
palindromeChainLength n = solve n 0 where
  solve n k = if n == pal n then k else solve (n + pal n) (k+1)
  pal = read . reverse . show
