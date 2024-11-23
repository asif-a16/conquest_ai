module PQueue where

import Data.List (unfoldr)

class PQueue pqueue where
  toPQueue   :: (a -> a -> Ordering) -> [a] -> pqueue a
  toPQueue cmp xs = foldr insert (empty cmp) xs

  fromPQueue :: pqueue a -> [a]
  fromPQueue = unfoldr detachMaybe

  priority :: pqueue a -> (a -> a -> Ordering)

  empty :: (a -> a -> Ordering) -> pqueue a
  isEmpty :: pqueue a -> Bool

  insert :: a -> pqueue a -> pqueue a

  extract :: pqueue a -> a
  discard :: pqueue a -> pqueue a
  detach  :: pqueue a -> (a, pqueue a)
  detach q = (extract q, discard q)

  detachMaybe :: pqueue a -> Maybe (a, pqueue a)
  detachMaybe q
    | isEmpty q = Nothing
    | otherwise = Just (detach q)
