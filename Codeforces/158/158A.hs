-- Codeforces 158A

main :: IO()
main = interact $ show . solve . map read . words

{-- interact:
 - interact :: (String -> String) -> IO()
 - The interact function takes a function of type String->String as its argument.
 - The entire input from the standard input device is passed to this function as 
 - its argument, and the resulting string is output on the standard output device.
--}

solve :: [Int] -> Int
solve (_:k:x) = sum[1 | i <- x, x!!(k-1) <= i, i > 0]

