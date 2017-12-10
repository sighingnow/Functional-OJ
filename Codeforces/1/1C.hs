-- Codeforces 1C

{-# LANGUAGE MultiWayIf #-}

import qualified Data.ByteString.Char8 as BC
import Data.Fixed (mod')
import Text.Printf

main :: IO ()
main = BC.getContents >>= solve . map (read . BC.unpack) . BC.words >>= printf "%.6f"

solve :: [Double] -> IO Double
solve [x1, y1, x2, y2, x3, y3] = do
    let ab = (x2 - x1, y2 - y1)
        ac = (x3 - x1, y3 - y1)
        bc = (x3 - x2, y3 - y2)
    let r1' = dist ab
        r2' = dist ac
        r3' = dist bc
        r1 = sqrt r1'
        r2 = sqrt r2'
        r3 = sqrt r3'
    let cc = (r1 + r2 + r3) / 2
        ss = sqrt (cc * (cc - r1) * (cc - r2) * (cc - r3)) -- 面积
        r = r1 * r2 * r3 / (4 * ss)                        -- 外接圆外径
    let delta1 = acos (1 - r1' / (2 * r * r))
        delta2 = acos (1 - r2' / (2 * r * r))
        delta3 = 2 * pi - delta1 - delta2
    let delta = fgcd delta1 (fgcd delta2 delta3)           -- 一条边对应的圆心角
        n = fromIntegral . round $ 2 * pi / delta
    return $ 0.5 * r * r * sin (2 * pi / n) * n

  where
    dist (u1, v1) = u1 * u1 + v1 * v1

    fgcd a b | a < 1e-4 = b
             | b < 1e-4 = a
             | otherwise = fgcd b (a `mod'` b)

