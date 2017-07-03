{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

import Control.Monad
import Data.List (foldl')
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L

-- | Data type definition.
data SimpleHeap = LfQ
                  | BrQ {-# UNPACK #-} !Int !SimpleHeap !SimpleHeap
    deriving (Show, Eq, Ord)

-- | Make an empty heap.
empty :: SimpleHeap
empty = LfQ

{-# INLINE empty #-}

-- | Make an heap of size 1 from single key-value pair.
singleton :: Int -> SimpleHeap
singleton k = BrQ k empty empty

{-# INLINE singleton #-}

-- | Query the key and value of the minimum element in the heap.
minKV :: SimpleHeap -> Int
minKV LfQ = error "Empty priority queue!"
minKV (BrQ k _ _) = k

{-# INLINE minKV #-}

-- | Whether the heap is empty.
isEmpty :: SimpleHeap -> Bool
isEmpty LfQ = True
isEmpty BrQ{} = False

{-# INLINE isEmpty #-}

-- | Insert new key-value pair into existing heap.
push :: Int -> SimpleHeap -> SimpleHeap
push k = union (singleton k)

{-# INLINE push #-}

-- | Pop up the top element in the heap.
pop :: SimpleHeap -> (Int, SimpleHeap)
pop LfQ = error "Empty priority queue!"
pop (BrQ k l r) = (k, l `union` r)

{-# INLINE pop #-}

-- | Merge two heap into one.
union :: SimpleHeap -> SimpleHeap -> SimpleHeap
union lhs@(BrQ kl _ _) rhs@(BrQ kr _ _)
    | kl <= kr = merge lhs rhs
    | otherwise = merge rhs lhs
  where
    merge LfQ _ = error "Runtime error!"
    merge (BrQ k LfQ r) t =
        BrQ k t r
    merge (BrQ k l r) t = BrQ k r (l `union` t)
union lhs LfQ = lhs
union LfQ rhs = rhs

{-# INLINE union #-}

fromList :: [Int] -> SimpleHeap
fromList = foldl' (flip push) empty

{-# INLINE fromList #-}

main :: IO ()
main = do
    [n, k] <- (map read . words) <$> getLine
    ns <- (foldl' (\h x -> push ((fst . fromJust . L.readInt) x) h) empty . L.split ' ') <$> L.getContents
    print $ perform 0 k ns

perform :: Int -> Int -> SimpleHeap -> Int
perform cnt k ns
  | a1 >= k = cnt
  | isEmpty h1 = -1
  | otherwise = let (a2, h2) = pop h1
                 in perform (cnt+1) k (push (a1 + 2 * a2) h2)
  where (a1, h1) = pop ns

