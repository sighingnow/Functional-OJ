import           Data.List

main :: IO ()
main = getContents >>= putStrLn . intercalate " " . map show . reverse . map (read :: String -> Int) . tail . words


