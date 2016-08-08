-- HackerRank challenge - Algorithms - 8-6: stock-maximize
import           Control.Monad ( unless )

main :: IO ()
main = getLine >>= loop . (read :: String -> Int)
  where
    loop n = unless (n == 0) $ do
        getLine
        fmap (map read . words) getLine >>= print . predict
        loop (n - 1)
    predict xs = sum $ zipWith (-) ms xs
      where
        ms = scanr1 max xs
