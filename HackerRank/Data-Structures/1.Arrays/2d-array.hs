import           Control.Monad
import           Data.List

main :: IO ()
main = do
    ns <- replicateM 6 ((map read . words) <$> getLine) :: IO [[Int]]
    let base = [(0, 0), (0, 1), (0, 2), (1, 1), (2, 0), (2, 1), (2, 2)]
    let calc a b = sum $ map (\(x, y) -> ns !! (a+x) !! (b+y)) base
    print $ maximum [calc a b | a <- [0..3], b <- [0..3]]

