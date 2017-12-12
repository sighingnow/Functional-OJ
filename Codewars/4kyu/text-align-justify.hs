module TextAlignJustify where

import Data.List

align :: [String] -> Int -> [String]
align [] _ = []
align words width = let (cs, r, ifEnd) = split [] 0 words width
                     in pretty ifEnd cs width : align r width
  where
    split r _ [] _ = (r, [], True)
    split r l (w:ws) width =
       let l' = length w
        in if l' + l > width
              then (r, (w:ws), False)
              else split (r ++ [w]) (l + l' + 1) ws width

    pretty True cs _ = intercalate " " cs
    pretty False [c] _ = c
    pretty False (c:cs) w =
      let l = sum $ map length (c:cs)
          (k1, k2) = (w - l) `divMod` (length cs)
       in c ++ (display (take k2 cs) (k1 + 1)) ++ (display (drop k2 cs) k1)
    display ws k = concatMap (\w -> replicate k ' ' ++ w) ws

justify :: String -> Int -> String
justify text = intercalate "\n" . align (words text)
