module ShortestPaths where

import Data.List (sortBy, (\\), find)
import Data.Map (Map)
import qualified Data.Map as M

import Game
import Graph
import PQueue
import PList
import Heap
import AdjList

dijkstra :: forall g e v pqueue. (Graph g e v, PQueue pqueue) => g -> [v] -> pqueue (Path e) -> [Path e]
dijkstra g [] ps = []
dijkstra g us ps
  | isEmpty ps  = []
  | t `elem` us =
      let us' :: [v]
          us' = undefined -- TODO: Problem 6
          ps'' :: pqueue (Path e)
          ps'' = undefined -- TODO: Problem 6
      in p : dijkstra g us' ps''
  | otherwise  = dijkstra g us ps'
  where
    (p, ps') = detach ps
    t = target p

shortestPaths :: forall g e v. Graph g e v => g -> v -> [Path e]
shortestPaths g v = dijkstra g (vertices g \\ [v]) ps
  where ps :: PList (Path e)
        ps = toPQueue cmpPath (map pathFromEdge (edgesFrom g v))

shortestPaths' :: forall g e v . Graph g e v => g -> v -> [Path e]
shortestPaths' g v = dijkstra g (vertices g \\ [v]) ps
  where ps :: Heap (Path e)
        ps = foldr (insert . pathFromEdge) (empty cmpPath) (edgesFrom g v)

shortestPath :: forall g e v . Graph g e v => v -> v -> g -> Maybe (Path e)
shortestPath src dst st = find ((== dst) . target) (shortestPaths st src)

shortestPath' :: forall g e v . Graph g e v => v -> v -> g -> Maybe (Path e)
shortestPath' src dst st = find ((== dst) . target) (shortestPaths' st src)

conflictZones :: GameState -> PlanetId -> PlanetId -> ([PlanetId], [PlanetId], [PlanetId])
conflictZones st p q = undefined -- TODO: Problem 10
