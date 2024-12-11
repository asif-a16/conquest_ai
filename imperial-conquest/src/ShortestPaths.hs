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

dijkstra :: forall g e v pqueue.
            (Graph g e v, PQueue pqueue)
         => g -> [v] -> pqueue (Path e) -> [Path e]
dijkstra g [] ps = []  -- No unvisited nodes, terminate
dijkstra g us ps
  | isEmpty ps = []  -- Priority queue empty, terminate
  | t `elem` us =
      let
        -- Updated list of unvisited nodes: remove the current target t
        us' :: [v]
        us' = filter (/= t) us

        -- Updated priority queue: add new candidate paths from the target node t
        ps'' :: pqueue (Path e)
        ps'' = foldr insert ps' newCandidates
          where
            -- Generate new candidate paths by extending p with edges from t
            newCandidates = [extend p e | e <- edgesFrom g t]
      in
        -- Add the current shortest path p and recursively compute the rest
        p : dijkstra g us' ps''
  | otherwise = dijkstra g us ps'  -- Skip visited node
  where
    -- Detach the shortest path p from the priority queue
    (p, ps') = detach ps
    -- Target vertex of the current shortest path
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
conflictZones gameState planetP planetQ =
  let
    -- Get the list of all planets from the game state
    allPlanets = vertices gameState

    -- Compute the shortest distances from planetP to all other planets
    distancesFromP = M.fromList 
      [ (targetPlanet, weight shortestPathToTarget) 
      | targetPlanet <- allPlanets, 
        Just shortestPathToTarget <- [shortestPath planetP targetPlanet gameState]
      ]

    -- Compute the shortest distances from planetQ to all other planets
    distancesFromQ = M.fromList 
      [ (targetPlanet, weight shortestPathToTarget) 
      | targetPlanet <- allPlanets, 
        Just shortestPathToTarget <- [shortestPath planetQ targetPlanet gameState]
      ]

    -- Function to classify a planet based on its distances from planetP and planetQ
    classifyPlanet targetPlanet =
      case (M.lookup targetPlanet distancesFromP, M.lookup targetPlanet distancesFromQ) of
        (Just distanceFromP, Just distanceFromQ)
          | distanceFromP < distanceFromQ  -> (True, False, False)  -- PlanetP reaches first
          | distanceFromQ < distanceFromP  -> (False, True, False)  -- PlanetQ reaches first
          | distanceFromP == distanceFromQ -> (False, False, True)  -- Both reach at the same time
        (Just _, Nothing) -> (True, False, False)  -- Only PlanetP can reach
        (Nothing, Just _) -> (False, True, False)  -- Only PlanetQ can reach
        _                 -> (False, False, False) -- Neither can reach

    -- Categorize planets into ps, qs, and pqs using fold
    (planetsReachedByP, planetsReachedByQ, planetsReachedSimultaneously) =
      foldr 
        (\currentPlanet (ps, qs, pqs) ->
          let (isReachedByP, isReachedByQ, isReachedSimultaneously) = classifyPlanet currentPlanet
          in ( if isReachedByP then currentPlanet : ps else ps,
               if isReachedByQ then currentPlanet : qs else qs,
               if isReachedSimultaneously then currentPlanet : pqs else pqs
             )
        )
        ([], [], [])
        allPlanets

  in
    (planetsReachedByP, planetsReachedSimultaneously, planetsReachedByQ)
