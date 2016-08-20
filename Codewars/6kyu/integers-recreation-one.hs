module Codewars.G964.Sumdivsq where

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n = filter (issquare . snd) $ map (\x -> (x, fx x)) [m..n] where
  intsqrt = floor . sqrt . fromIntegral
  fx n = (sum . map (\x -> x*x) . factors) n - if k*k==n then n else 0 where
    k = intsqrt n
    factors n = map (n `div`) xs ++ xs where xs = filter (\k -> rem n k == 0) [1..k]
  issquare n = k*k==n where k = intsqrt n
