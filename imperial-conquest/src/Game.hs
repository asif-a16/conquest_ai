{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGe DeriveAnyClass #-}

module Game where

import Data.Maybe (fromJust)
import Data.Array (Ix)
import Graph

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Binary as B
import           GHC.Generics

data Player = Player1 | Player2 deriving (Eq, Ord, Show, Read, Generic, B.Binary)

data Planet = Planet Owner Ships Growth deriving (Show, Read, Generic, B.Binary)

newtype Ships = Ships Int
  deriving stock (Ord, Eq, Ix, Generic)
  deriving newtype (Num, B.Binary, Show, Read) -- don't print or read constructor

newtype Growth = Growth Int
  deriving stock (Ord, Eq)
  deriving newtype (Num, B.Binary, Show, Read, Enum, Integral, Real) -- don't print or read constructor

data Owner = Neutral | Owned Player deriving (Eq, Show, Read, Generic, B.Binary)

newtype PlanetId = PlanetId Int
  deriving stock (Ord, Eq, Generic)
  deriving newtype (Num, B.Binary, Show, Read) -- don't print or read constructor

type Planets = Map PlanetId Planet

data Wormhole = Wormhole Source Target Turns deriving (Eq, Show, Read, Generic, B.Binary)

newtype Source = Source PlanetId
  deriving stock (Ord, Eq)
  deriving newtype (B.Binary, Show, Read) -- don't print or read constructor

newtype Target = Target PlanetId
  deriving stock (Ord, Eq)
  deriving newtype (B.Binary, Show, Read) -- don't print or read constructor

newtype Turns  = Turns Int
  deriving stock (Ord, Eq)
  deriving newtype (Num, B.Binary, Show, Read) -- don't print or read constructor

newtype WormholeId = WormholeId Int
  deriving stock (Ord, Eq, Generic)
  deriving newtype (B.Binary, Show, Read) -- don't print or read constructor

type Wormholes = Map WormholeId Wormhole

data Fleet = Fleet Player Ships WormholeId Turns deriving (Show, Read, Generic, B.Binary)

type Fleets = [Fleet]

data GameState = GameState Planets Wormholes Fleets deriving (Show, Read, Generic, B.Binary)

data Order = Order WormholeId Ships deriving (Eq, Ord, Show, Read, Generic, B.Binary)

wormholesFrom :: Source -> GameState -> Wormholes
wormholesFrom pId (GameState _ ws _) = M.filter (\(Wormhole s _ _) -> s == pId) ws

wormholesTo :: Target -> GameState -> Wormholes
wormholesTo pId (GameState _ ws _) = M.filter (\(Wormhole _ t _) -> t == pId) ws

targetPlanets :: GameState -> Source -> [(PlanetId, Ships, Growth)]
targetPlanets st s = map (planetDetails . target) (M.elems (wormholesFrom s st))
  where planetDetails :: PlanetId -> (PlanetId, Ships, Growth)
        planetDetails pId = (pId, ships, growth)
          where Planet _ ships growth = lookupPlanet pId st

shipsOnPlanet :: GameState -> PlanetId -> Ships
shipsOnPlanet st pId = ships
  where Planet _ ships _ = lookupPlanet pId st

lookupPlanet :: PlanetId -> GameState -> Planet
lookupPlanet pId (GameState ps _ _) = ps M.! pId

enemyPlanet :: Planet -> Bool
enemyPlanet (Planet (Owned Player2) _ _) = True
enemyPlanet _                            = False

ourPlanet :: Planet -> Bool
ourPlanet (Planet (Owned Player1) _ _) = True
ourPlanet _                            = False

ourPlanets :: GameState -> Planets
ourPlanets (GameState ps _ _) = M.filter ourPlanet ps

enemyPlanets :: GameState -> Planets
enemyPlanets (GameState ps _ _) = M.filter enemyPlanet ps

lookupWormhole :: WormholeId -> GameState -> Wormhole
lookupWormhole wId (GameState _ wormholes _) = wormholes M.! wId

instance Edge Wormhole PlanetId where
  source (Wormhole (Source s) _ _)    = s
  target (Wormhole _ (Target t) _)    = t
  weight (Wormhole _ _ (Turns turns)) = toInteger turns

instance Edge (WormholeId, Wormhole) PlanetId where
  source (_, w) = source w
  target (_, w) = target w
  weight (_, w) = weight w

instance Graph GameState (WormholeId, Wormhole) PlanetId where
  vertices (GameState ps _ _) = M.keys ps
  edges    (GameState _ ws _) = M.assocs ws
  edgesTo   st pId = M.toList (wormholesTo (Target pId) st)
  edgesFrom st pId = M.toList (wormholesFrom (Source pId) st)
  velem pId      (GameState ps _ _) = M.member pId ps
  eelem (wId, _) (GameState _ ws _) = M.member wId ws
