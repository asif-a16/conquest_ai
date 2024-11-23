module Util where

import Data.Array

-- tabulate
tabulate :: Ix i => (i,i) -> (i -> a) -> Array i a
tabulate (u,v) f = array (u,v) [ (i, f i) | i <- range (u, v)]

-- Comparator helpers
lt :: (a -> a -> Ordering) -> (a -> a -> Bool)
lt cmp x y = cmp x y == LT

gt :: (a -> a -> Ordering) -> (a -> a -> Bool)
gt cmp x y = cmp x y == GT

lte :: (a -> a -> Ordering) -> (a -> a -> Bool)
lte cmp x y = cmp x y /= GT

eq :: (a -> a -> Ordering) -> (a -> a -> Bool)
eq cmp x y = cmp x y == EQ

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy f x y
 | f x > f y = x
 | otherwise = y

maximumElse :: Ord a => a -> [a] -> a
maximumElse = foldr max

headMaybe :: [a] -> Maybe a
headMaybe = foldr (const . Just) Nothing
