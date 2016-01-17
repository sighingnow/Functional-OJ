-- HackerRank challenge - Algorithms - 1-9: library fine

main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Int] -> Int
solve [d1, m1, y1, d2, m2, y2]
    | y1 < y2 || (y1 == y2 && m1 < m2) || (y1 == y2 && m1 == m2 && d1 <= d2) = 0
    | y1 == y2 && m1 == m2 && d1 > d2 = 15 * (d1-d2)
    | y1 == y2 && m1 > m2 = 500 * (m1-m2)
    | y1 > y2 = 10000
