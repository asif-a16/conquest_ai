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
mergeTree cmp l r = undefined -- TODO: Problem 7

instance PQueue Heap where
  priority :: Heap a -> (a -> a -> Ordering)
  priority = undefined -- TODO: Problem 8

  empty :: (a -> a -> Ordering) -> Heap a
  empty p = undefined -- TODO: Problem 8

  isEmpty :: Heap a -> Bool
  isEmpty = undefined -- TODO: Problem 8

  insert :: a -> Heap a -> Heap a
  insert = undefined -- TODO: Problem 8

  extract :: Heap a -> a
  extract = undefined -- TODO: Problem 8

  discard :: Heap a -> Heap a
  discard = undefined -- TODO: Problem 8
