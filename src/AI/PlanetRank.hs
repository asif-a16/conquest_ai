{-# LANGUAGE DerivingStrategies, GeneralisedNewtypeDeriving #-}
module AI.PlanetRank where

import Game
import Graph

import Data.Map (Map)
import qualified Data.Map as M
import Text.Printf (printf)

newtype PlanetRank = PlanetRank Double
  deriving (Num, Eq, Ord, Fractional)
  deriving newtype (Read)

type PlanetRanks = Map PlanetId PlanetRank

instance Show PlanetRank where
  show (PlanetRank p) = printf "%.4f" p

initPlanetRanks :: GameState -> PlanetRanks
initPlanetRanks g = M.fromList [(p, PlanetRank (1 / fromIntegral n)) | p <- ps]
  where ps = vertices g
        n  = length ps

planetRank :: GameState -> PlanetRanks
planetRank g = planetRanks g !! 200

planetRanks :: GameState -> [PlanetRanks]
planetRanks g = iterate (nextPlanetRanks g) (initPlanetRanks g)

nextPlanetRanks :: GameState -> PlanetRanks -> PlanetRanks
nextPlanetRanks g pr = M.mapWithKey (const . nextPlanetRank g pr) pr

nextPlanetRank :: GameState -> PlanetRanks -> PlanetId  -> PlanetRank
nextPlanetRank g@(GameState planets wormholes fleets) pr i =
  (1 - d) / n + d * sum [ pr M.! j * growth i / growths j | j <- targets i ]
  where d   = 0.85
        n   = fromIntegral (length planets)

        targets :: PlanetId -> [PlanetId]
        targets planetId = map target (M.elems (wormholesFrom (Source planetId) g))

        growth :: PlanetId -> PlanetRank
        growth i  = (\(Planet _ _ g) -> fromIntegral g) (planets M.! i)

        growths :: PlanetId -> PlanetRank
        growths planetId = sum (map (growth . source) (M.elems (wormholesTo (Target planetId) g)))

checkPlanetRanks :: PlanetRanks -> PlanetRank
checkPlanetRanks = sum . M.elems
