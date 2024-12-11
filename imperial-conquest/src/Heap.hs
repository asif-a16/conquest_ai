module Heap (Heap, rankHeap, mergeHeap, module PQueue) where

import PQueue
import Util

data Heap a = Heap (a -> a -> Ordering) (Tree a)
data Tree a = Nil | Node Int (Tree a) a (Tree a)

rankTree :: Tree a -> Int
rankTree Nil            = 0
rankTree (Node h l x r) = h

rankHeap :: Heap a -> Int
rankHeap (Heap _ t) = rankTree t

node :: Tree a -> a -> Tree a -> Tree a
node l x r
  | hl < hr   = Node (hl + 1) r x l
  | otherwise = Node (hr + 1) l x r
 where
  hl = rankTree l
  hr = rankTree r

mergeHeap :: Heap a -> Heap a -> Heap a
mergeHeap (Heap cmp l) (Heap _ r) = Heap cmp (mergeTree cmp l r)

mergeTree :: (a -> a -> Ordering) -> Tree a -> Tree a -> Tree a
mergeTree _ Nil rightTree = rightTree  -- If the first tree is empty, return the second tree
mergeTree _ leftTree Nil = leftTree    -- If the second tree is empty, return the first tree
mergeTree cmp leftTree@(Node _ leftLeft rootLeft leftRight) rightTree@(Node _ rightLeft rootRight rightRight)
  | cmp rootLeft rootRight == GT = mergeTree cmp rightTree leftTree  -- Ensure smaller root becomes the new root
  | otherwise =
      let mergedRight = mergeTree cmp leftRight rightTree  -- Recursively merge the right subtree of the smaller root
      in node leftLeft rootLeft mergedRight                -- Create a new node, ensuring balance with the `node` function

instance PQueue Heap where
  -- Retrieve the priority comparison function
  priority :: Heap a -> (a -> a -> Ordering)
  priority (Heap cmp _) = cmp

  -- Create an empty heap with a given comparison function
  empty :: (a -> a -> Ordering) -> Heap a
  empty cmp = Heap cmp Nil

  -- Check if the heap is empty
  isEmpty :: Heap a -> Bool
  isEmpty (Heap _ Nil) = True
  isEmpty _ = False

  -- Insert an element into the heap
  insert :: a -> Heap a -> Heap a
  insert x (Heap cmp tree) = mergeHeap (Heap cmp (Node 1 Nil x Nil)) (Heap cmp tree)

  -- Extract the root element (minimum/maximum depending on cmp)
  extract :: Heap a -> a
  extract (Heap _ Nil) = error "extract: Empty heap"
  extract (Heap _ (Node _ _ x _)) = x

  -- Discard the root element and adjust the heap
  discard :: Heap a -> Heap a
  discard (Heap _ Nil) = error "discard: Empty heap"
  discard (Heap cmp (Node _ l _ r)) = Heap cmp (mergeTree cmp l r)

