module AdjList where

import Graph
import Data.List (nub, concatMap)

newtype AdjList e v = AdjList [(v, [e])] deriving Show

instance (Eq e, Edge e v) => Graph (AdjList e v) e v where
  vertices (AdjList ves) = nub (concat [[v] ++ [target e | e <- es] | (v, es) <- ves])

  edges (AdjList ves) = concatMap snd ves

  -- Return the list of edges originating from the given source vertex
  edgesFrom (AdjList ves) s = case lookup s ves of
    Just es -> es
    Nothing -> []

  -- Return the list of edges targeting the given vertex
  edgesTo (AdjList ves) t = [e | (_, es) <- ves, e <- es, target e == t]

  -- Check if a vertex is in the graph
  velem v (AdjList ves) = v `elem` vertices (AdjList ves)

  -- Check if an edge is in the graph
  eelem e (AdjList ves) = e `elem` edges (AdjList ves)
