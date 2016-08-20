module BinaryTreeTraversal
  ( preOrder
  , inOrder
  , postOrder
  ) where

import BinaryTreeTraversal.Types

{-
data Tree a = Nil | Node (Tree a) a (Tree a)
-}

-- 1.) Root node, 2.) traverse left subtree, 3.) traverse right subtree.
preOrder :: Tree a -> [a]
preOrder Nil = []
preOrder (Node l x r) = [x] ++ (preOrder l) ++ (preOrder r)

-- 1.) Traverse left subtree, 2.) root node, 3.) traverse right subtree.
inOrder :: Tree a -> [a]
inOrder Nil = []
inOrder (Node l x r) = (inOrder l) ++ [x] ++ (inOrder r)

-- 1.) Traverse left subtree, 2.) traverse right subtree, 3.) root node.
postOrder :: Tree a -> [a]
postOrder Nil = []
postOrder (Node l x r) = (postOrder l) ++ (postOrder r) ++ [x]
