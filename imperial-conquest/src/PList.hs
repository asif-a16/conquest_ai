module PList (PList, module PQueue) where

import PQueue
import Data.List (sortBy)
import Util (lte)

data PList a = PList (a -> a -> Ordering) [a]

instance Show a => Show (PList a) where
  show (PList _ xs) = show xs

instance PQueue PList where
  toPQueue cmp xs = PList cmp (sortBy cmp xs)

  fromPQueue (PList _ xs) = xs

  empty cmp = PList cmp []

  isEmpty (PList _ xs) = null xs

  priority (PList cmp _) = cmp

  insert x (PList cmp []) = PList cmp [x]
  insert x ps@(PList cmp xs)
    | x <= y    = cons x ps
    | otherwise = cons y (insert x ys)
    where (<=) = lte cmp
          (y, ys) = detach ps
          cons x (PList cmp xs) = PList cmp (x:xs)

  extract (PList cmp (x:xs)) = x
  extract _ = error "extract from empty queue"

  discard (PList cmp (x:xs)) = PList cmp xs
  discard _ = error "discard from empty queue"
