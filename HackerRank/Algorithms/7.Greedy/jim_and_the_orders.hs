-- HackerRank challenge - Algorithms - 7-13: jim and the orders

import           Control.Monad
import           Data.Function
import           Data.List

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    ns <- replicateM n ((sum . map read . words) <$> getLine) :: IO [Int]
    let r = map fst . sortBy (compare `on` snd) $ zip [1..] ns
    putStrLn . unwords . map show $ r
