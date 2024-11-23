module AdjList where

import Graph

newtype AdjList e v = AdjList [(v, [e])] deriving Show

instance (Eq e, Edge e v) => Graph (AdjList e v) e v where
  vertices  (AdjList ves)   = undefined -- TODO: Problem 9
  edges     (AdjList ves)   = undefined -- TODO: Problem 9
  edgesFrom (AdjList ves) s = undefined -- TODO: Problem 9
  edgesTo   (AdjList ves) t = undefined -- TODO: Problem 9
  velem v   (AdjList ves)   = undefined -- TODO: Problem 9
  eelem e   (AdjList ves)   = undefined -- TODO: Problem 9
