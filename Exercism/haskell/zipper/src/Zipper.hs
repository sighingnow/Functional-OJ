module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

data TreeLoc a
    = Top
    | GoL a (TreeLoc a) (Maybe (BinTree a))
    | GoR a (TreeLoc a) (Maybe (BinTree a))
    deriving (Eq, Show)

data Zipper a = Zipper (TreeLoc a) (BinTree a) deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree = Zipper Top

toTree :: Zipper a -> BinTree a
toTree (Zipper Top t) = t
toTree (Zipper (GoL a loc tr) tl) = toTree (Zipper loc (BT a (Just tl) tr))
toTree (Zipper (GoR a loc tl) tr) = toTree (Zipper loc (BT a tl (Just tr)))

value :: Zipper a -> a
value (Zipper _ (BT v _ _)) = v

left :: Zipper a -> Maybe (Zipper a)
left (Zipper _ (BT _ Nothing _)) = Nothing
left (Zipper loc (BT v (Just tl) tr)) = Just $ Zipper (GoL v loc tr) tl

right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ (BT _ _ Nothing)) = Nothing
right (Zipper loc (BT v tl (Just tr))) = Just $ Zipper (GoR v loc tl) tr

up :: Zipper a -> Maybe (Zipper a)
up (Zipper Top _) = Nothing
up (Zipper (GoL v loc tr) tree) = Just $ Zipper loc (BT v (Just tree) tr)
up (Zipper (GoR v loc tl) tree) = Just $ Zipper loc (BT v tl (Just tree))

setValue :: a -> Zipper a -> Zipper a
setValue v (Zipper loc (BT _ tl tr)) = Zipper loc (BT v tl tr)

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft tl (Zipper loc (BT v _ tr)) = Zipper loc (BT v tl tr)

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight tr (Zipper loc (BT v tl _)) = Zipper loc (BT v tl tr)
