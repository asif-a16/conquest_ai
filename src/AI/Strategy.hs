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

-- PlanetRank Rush Strategy
planetRankRush :: GameState -> AI.State -> ([Order], Log, AI.State)
planetRankRush = undefined -- TODO: Problem 9

-- TimidRush Strategy
timidAttackFromAll :: PlanetId -> GameState -> [Order]
timidAttackFromAll = undefined -- TODO: Problem 10

timidRush :: GameState -> AI.State -> ([Order], Log, AI.State)
timidRush = undefined -- TODO: Problem 10

-- Skynet
skynet :: GameState -> AI.State -> ([Order], Log, AI.State)
skynet = undefined
