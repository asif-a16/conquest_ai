module AI.Strategy where

import AI.State as AI
import AI.PlanetRank (planetRank, PlanetRanks)
import Game
import Graph
import ShortestPaths
import Util

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

type Log = [String]

-- you can feel free to add to this datatype, the `clever-ai` uses `strategise`
-- and the server uses `defaultStrategy` only.
data Strategy = Pacifist
              | ZergRush
              | PlanetRankRush
              | TimidRush
              | Skynet -- you should retain this, in case we pass "Skynet" for any competition
              deriving (Enum, Bounded, Show, Read)

defaultStrategy :: Strategy
defaultStrategy = Pacifist

strategise :: Strategy -> (GameState -> AI.State -> ([Order], Log, AI.State))
strategise Pacifist = pacifist
strategise ZergRush = zergRush
strategise PlanetRankRush = planetRankRush
strategise TimidRush = timidRush
strategise Skynet = skynet

-- Helpers
findEnemyPlanet :: GameState -> Maybe PlanetId
findEnemyPlanet (GameState ps _ _) =
      case M.toList $ M.filter enemyPlanet ps of
            ((planetId, _):_) -> Just planetId
            []                -> Nothing

send :: WormholeId -> Maybe Ships -> GameState -> [Order]
send wId mShips st
      | ourPlanet planet =
            case mShips of
                  Nothing -> [Order wId total_ships]
                  Just _  -> [Order wId (min total_ships (fromJust mShips))]
      | otherwise        = []
      where Wormhole (Source src) _ _ = lookupWormhole wId st
            planet@(Planet _ total_ships _) = lookupPlanet src st

-- Pacifist Strategy
pacifist :: GameState -> AI.State -> ([Order], Log, AI.State)
pacifist _ ai = ([], ["This world is illusory. Why fight?"], ai)

-- Zerg Rush Strategy
attackFromAll :: PlanetId -> GameState -> [Order]
attackFromAll targetId gs =
  -- Generate orders to send ships from each planet along the last edge of their path to the target
  concatMap ((\edge -> send edge Nothing gs) . fst) (helper (catMaybes temp))
  where
    -- Compute the shortest paths from each of our planets to the target planet
    temp = map
      (\(planetId, _) -> shortestPath planetId targetId gs)  -- Find the shortest path from each planet to the target
      (M.toList (ourPlanets gs))                            -- List of our planets with their IDs and details

    -- Extract the last edge of each valid path to the target
    helper :: [Path e] -> [e]
    helper [] = []                                           -- No paths, return an empty list
    helper ((Path _ edges) : paths) =
      last edges : helper paths                              -- Add the last edge of the current path and recurse for the rest

zergRush :: GameState -> AI.State -> ([Order], Log, AI.State)
zergRush gameState aiState
  -- If there's no target planet, find a new enemy planet to target
  | isNothing currentTarget = ([], [], aiState {rushTarget = findEnemyPlanet gameState})

  -- If the target planet is already ours, find a new enemy planet to target
  | ourPlanet (lookupPlanet (fromJust currentTarget) gameState) =
      ([], [], aiState {rushTarget = findEnemyPlanet gameState})

  -- Otherwise, attack the current target planet
  | otherwise = (attackFromAll (fromJust currentTarget) gameState, [], aiState)
  where
    currentTarget = rushTarget aiState  -- The current target planet from the AI's state

findBestTarget :: PlanetRanks -> GameState -> Maybe PlanetId
findBestTarget planetRanks gameState
      | M.null candidates = Nothing
      | otherwise = Just (minimum [id | (id, r) <- M.toList candidates, r == maximum (M.elems candidates)])
      where  candidates = M.filterWithKey (\id _ -> not (ourPlanet (lookupPlanet id gameState))) planetRanks

-- PlanetRank Rush Strategy
planetRankRush :: GameState -> AI.State -> ([Order], Log, AI.State)
planetRankRush gameState aiState =
  (generatedOrders, [], aiState { rushTarget = updatedTarget, prs = Just planetRanks })
  where
    -- Retrieve or calculate the current planet ranks
    planetRanks = fromMaybe (planetRank gameState) (prs aiState)

    -- Find the best target planet based on the planet ranks
    bestTargetPlanet = findBestTarget planetRanks gameState

    -- Generate orders and determine the updated target
    (generatedOrders, updatedTarget) = case bestTargetPlanet of
      Just planetId
        | not (ourPlanet (lookupPlanet planetId gameState)) ->
            (attackFromAll planetId gameState, Just planetId)  -- Attack the target if it's not owned
      _ ->
            ([], bestTargetPlanet)  -- No valid target or already owned, do nothing

-- Find the easiest path between two planets based on defense strength
easiestPath :: PlanetId -> PlanetId -> GameState -> Maybe (Path (WormholeId, Wormhole))
easiestPath sourcePlanet targetPlanet gameState
  | null validPaths = Nothing  -- No valid paths found
  | otherwise       = Just (minimumBy comparePaths validPaths)
  where
    -- Filter paths from source to target that end at the target planet
    validPaths :: [Path (WormholeId, Wormhole)]
    validPaths = filter ((== targetPlanet) . target) 
                        (shortestPaths (defState gameState) sourcePlanet)

    -- Calculate the defense value of a path based on enemy ships on target planets
    pathDefenseValue :: Path (WormholeId, Wormhole) -> GameState -> Int
    pathDefenseValue (Path _ edges) currentGameState =
      sum [ numShips
          | (_, wormhole) <- edges
          , let Ships numShips = shipsOnPlanet currentGameState (target wormhole)
          , not (ourPlanet (lookupPlanet (target wormhole) currentGameState))
          ]

    -- Compare two paths based on their defense value and the target of the last edge
    comparePaths :: Path (WormholeId, Wormhole)
                 -> Path (WormholeId, Wormhole)
                 -> Ordering
    comparePaths path1@(Path _ edges1) path2@(Path _ edges2) =
      compare (pathDefenseValue path1 gameState) (pathDefenseValue path2 gameState)
        <> compare (target (snd (last edges1)))
                   (target (snd (last edges2)))

-- TimidRush Strategy
defState :: GameState -> GameState
defState gameState@(GameState planets wormholes fleets) =
  GameState planets updatedWormholes fleets
  where
    -- Update the delay for each wormhole
    updatedWormholes = M.map updateWormhole wormholes

    -- Recalculate the delay for a single wormhole
    updateWormhole :: Wormhole -> Wormhole
    updateWormhole wormhole@(Wormhole sourcePlanet targetPlanet _)
      -- If the target planet is owned by us, set the delay to 0
      | ourPlanet (lookupPlanet (target wormhole) gameState) =
          Wormhole sourcePlanet targetPlanet (Turns 0)

      -- Otherwise, set the delay based on the number of ships on the target planet
      | otherwise =
          Wormhole sourcePlanet targetPlanet (Turns delayTurns)
      where
        -- Extract the number of ships on the target planet
        Ships delayTurns = shipsOnPlanet gameState (target wormhole)

-- Generate attack orders from all owned planets to the target planet
timidAttackFromAll :: PlanetId -> GameState -> [Order]
timidAttackFromAll targetPlanetId gameState =
  concatMap ((\edge -> send edge Nothing gameState) . fst) validPaths
  where
    -- Calculate the shortest paths from each owned planet to the target planet
    shortestPathsToTarget = 
      map (\(planetId, _) -> easiestPath planetId targetPlanetId gameState) 
          (M.toList (ourPlanets gameState))

    -- Filter valid paths (non-Nothing) and extract the last edge of each path
    validPaths = extractLastEdges (catMaybes shortestPathsToTarget)

    -- Extract the last edge from each path in the list
    extractLastEdges :: [Path e] -> [e]
    extractLastEdges [] = []
    extractLastEdges ((Path _ edges) : remainingPaths) = 
      last edges : extractLastEdges remainingPaths

-- Execute a timid rush strategy based on the current game state and AI state
timidRush :: GameState -> AI.State -> ([Order], Log, AI.State)
timidRush gameState aiState =
  (generatedOrders, [], updatedAIState)
  where
    -- Retrieve or calculate the current planet ranks
    planetRanks = fromMaybe (planetRank gameState) (prs aiState)

    -- Identify the best target planet based on the planet ranks
    bestTargetPlanet = findBestTarget planetRanks gameState

    -- Determine orders and update the target based on the best target planet
    (generatedOrders, updatedTarget) = case bestTargetPlanet of
      Just targetPlanetId
        | not (ourPlanet (lookupPlanet targetPlanetId gameState)) ->
            (timidAttackFromAll targetPlanetId gameState, Just targetPlanetId)
      _ ->
            ([], bestTargetPlanet)

    -- Update the AI state with the new target and current planet ranks
    updatedAIState = aiState { rushTarget = updatedTarget, prs = Just planetRanks }

-- Skynet
skynet :: GameState -> AI.State -> ([Order], Log, AI.State)
skynet = undefined
