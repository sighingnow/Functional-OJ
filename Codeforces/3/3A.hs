-- Codeforces 3A

import           Control.Monad
import           Data.Char
import           Data.List

main :: IO ()
main = do
    [s, t] <- mapM (fmap ((\(x, y) -> (ord $ head x, read y)) . splitAt 1)) [getLine, getLine]
    let ans = solve s t
    putStrLn $ intercalate "\n" $ (show (length ans)) : ans

solve :: (Int, Int) -> (Int, Int) -> [String]
solve (sx, sy) (tx, ty)
    | sx == tx && sy <= ty = replicate (ty-sy) "U"
    | sx == tx && sy >= ty = replicate (sy-ty) "D"
    | sx >= tx && sy == ty = replicate (sx-tx) "L"
    | sx <= tx && sy == ty = replicate (tx-sx) "R"
    | sx < tx && sy < ty && (tx-sx) <= (ty-sy) = (replicate (tx-sx) "RU") ++ (replicate ((ty-sy)-(tx-sx)) "U")
    | sx < tx && sy < ty && (tx-sx) >= (ty-sy) = (replicate (ty-sy) "RU") ++ (replicate ((tx-sx)-(ty-sy)) "R")
    | sx < tx && sy > ty && (tx-sx) >= (sy-ty) = (replicate (sy-ty) "RD") ++ (replicate ((tx-sx)-(sy-ty)) "R")
    | sx < tx && sy > ty && (tx-sx) <= (sy-ty) = (replicate (tx-sx) "RD") ++ (replicate ((sy-ty)-(tx-sx)) "D")
    | sx > tx && sy > ty && (sx-tx) <= (sy-ty) = (replicate (sx-tx) "LD") ++ (replicate ((sy-ty)-(sx-tx)) "D")
    | sx > tx && sy > ty && (sx-tx) >= (sy-ty) = (replicate (sy-ty) "LD") ++ (replicate ((sx-tx)-(sy-ty)) "L")
    | sx > tx && sy < ty && (sx-tx) >= (ty-sy) = (replicate (ty-sy) "LU") ++ (replicate ((sx-tx)-(ty-sy)) "L")
    | sx > tx && sy < ty && (sx-tx) <= (ty-sy) = (replicate (sx-tx) "LU") ++ (replicate ((ty-sy)-(sx-tx)) "U")
