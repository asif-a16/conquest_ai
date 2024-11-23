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
findEnemyPlanet = undefined -- TODO: Problem 1

send :: WormholeId -> Maybe Ships -> GameState -> [Order]
send wId mShips st = undefined -- TODO: Problem 2
 where Wormhole (Source src) _ _ = lookupWormhole wId st
       planet@(Planet _ total_ships _) = lookupPlanet src st

-- Pacifist Strategy
pacifist :: GameState -> AI.State -> ([Order], Log, AI.State)
pacifist _ ai = ([], ["This world is illusory. Why fight?"], ai)

-- Zerg Rush Strategy
attackFromAll :: PlanetId -> GameState -> [Order]
attackFromAll targetId gs = undefined -- TODO: Problem 3

zergRush :: GameState -> AI.State -> ([Order], Log, AI.State)
zergRush = undefined -- TODO: Problem 4

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
