{-# OPTIONS_GHC -O3 #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

import Control.Monad
import Data.List (foldl')
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L

import Prelude hiding (head)

-- | Model defination: SBT.
--
--      * @Nil@
--      * @Branch val cnt left right@
data SBT a = Nil
           | Branch { val   :: a
                    , cnt   :: Int
                    , left  :: SBT a
                    , right :: SBT a
                    }
    deriving (Eq, Show)

{------------------------------------------------------------------
  Construction
-------------------------------------------------------------------}
-- | /O(1)/. The empty SBT.
empty :: SBT a
empty = Nil

-- | /O(1)/. A SBT with a single element.
singleton :: a -> SBT a
singleton v = Branch v 1 Nil Nil

-- | /O(n (log n))/. Construct Treap from list[val].
fromList :: (Ord a) => [a] -> SBT a
fromList = foldl (flip insert) empty

-- | /O(n)/. Tranform a Treap to a list.
toList :: SBT a -> [a]
toList Nil = []
toList (Branch v _ l r) =
    toList l ++ [ v ] ++ toList r

{----------------------------------------------------------------
  Validate.
------------------------------------------------------------------}
-- | Validate the laws of SBT:
--
-- @
--                      T
--                     \/ \\
--                    L   R
--                   \/ \\ \/ \\
--                  A  B C  D
-- @
--
-- These are tow properties of the SBT T:
--
--      * size(R) >= size(A), size(B)
--      * size(L) >= size(C), size(D)
--
validate :: (Ord a) => SBT a -> Bool
validate Nil = True
validate (Branch v s l r) =
    (s == size l + size r + 1)
        && (l == Nil || (v >= val l) && size r >= size (left l) && size r >= size (right l))
            && (r == Nil || (v <= val r) && size l >= size (left r) && size l >= size (right r))
                && validate l
                    && validate r

{--------------------------------------------------------------
  Query
--------------------------------------------------------------}
-- | /O(1)/. Is the SBT empty ?
null :: SBT a -> Bool
null Nil = True
null Branch{} = False

-- | /O(n)/. Ihe number of elements in the SBT.
size :: SBT a -> Int
size Nil = 0
size (Branch _ s _ _) = s

-- | /O(log n)/. Get the minimum element of the SBT.
head :: SBT a -> a
head Nil = error "SBT.head: empty SBT, no such element."
head (Branch v _ Nil _) =
    v
head (Branch _ _ l _) = head l

{-------------------------------------------------------------------------------
  Maintain.
--------------------------------------------------------------------------------}
-- | /O(1)/. Left-Rotate.
rotateL :: SBT a -> SBT a
rotateL Nil = Nil
rotateL (Branch v s l r) =
    Branch (val r) s (Branch v (size l + size (left r) + 1) l (left r)) (right r)

-- | /O(1)/. Right-Rotate.
rotateR :: SBT a -> SBT a
rotateR Nil = Nil
rotateR (Branch v s l r) =
    Branch (val l) s (left l) (Branch v (size r + size (right l) + 1) (right l) r)

-- | /O(1)/. Maintian the SBT after insert or delete a node. The average time complexity
-- of the maintian operation is /O(1)/.
maintain :: (Ord a) => SBT a -> SBT a
maintain Nil = Nil
maintain (Branch v s l r)
    | l /= Nil && size (left l) > size r =
          let t = rotateR (Branch v s l r)
          in
              maintain (Branch (val t) (cnt t) (left t) (maintain $ right t))
    | l /= Nil && size (right l) > size r =
          let t = rotateR (Branch v s (rotateL l) r)
          in
              maintain (Branch (val t) (cnt t) (maintain (left t)) (maintain (right t)))
    | r /= Nil && size (right r) > size l =
          let t = rotateL (Branch v s l r)
          in
              maintain (Branch (val t) (cnt t) (maintain $ left t) (right t))
    | r /= Nil && size (left r) > size l =
          let t = rotateL (Branch v s l (rotateR r))
          in
              maintain (Branch (val t) (cnt t) (maintain (left t)) (maintain (right t)))
    | otherwise = Branch v s l r -- do nothing.

{------------------------------------------------------------------
  Operation
-------------------------------------------------------------------}
-- | /O(log n)/. Insert a new element of value @x@ and fix @fx@ in the SBT.
insert :: (Ord a) => a -> SBT a -> SBT a
insert x Nil = singleton x
insert x (Branch v s l r) =
    maintain t
  where
    t = if x > v then Branch v (s + 1) l (insert x r) else Branch v (s + 1) (insert x l) r

-- | /O(log n)/. Remove the element from the treap, if the elements doesn't exist, do noting.
remove :: (Ord a) => a -> SBT a -> SBT a
remove = maintain .: rm
  where
    rm _ Nil = Nil
    rm x (Branch v s l r)
        | x > v = Branch v (s - 1) l (maintain $ rm x r)
        | x < v = Branch v (s - 1) (maintain $ rm x l) r
        | otherwise = case (l, r) of
              (Nil, _r) -> _r
              (_l, Nil) -> _l
              (_l, _r) -> let (e, es) = ml _l
                          in
                              Branch e (s - 1) es _r
                  where
                        -- ml: maximum element of left branch and the other subtree.
                        ml Nil = error "SBT.remove: error occurred during the `remove` operation."
                        ml (Branch lv ls ll lr)
                            | lr == Nil = (lv, ll)
                            | otherwise = let (_e, _es) = ml lr
                                          in
                                              (_e, maintain (Branch lv (ls - 1) ll _es))

    -- compose two functions.
    (.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
    (.:) f g x y = f (g x y)

main :: IO ()
main = do
    _ <- getLine
    ns <- (map (fst . fromJust . L.readInt) . L.words) <$> L.getContents
    print ns
    perform empty ns

perform :: SBT Int -> [Int] -> IO ()
perform h (3:xs) = print (head h) >> perform h xs
perform h (2:x:xs) = perform (remove x h) xs
perform h (1:x:xs) = perform (insert x h) xs

