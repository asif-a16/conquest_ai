module Examples where

import Game

import qualified Data.Map as M

example1 :: GameState
example1 = GameState planets wormholes fleets
  where planets = M.fromList
          [ (PlanetId 0, Planet (Owned Player1) (Ships 300) (Growth 0))
          , (PlanetId 1, Planet Neutral         (Ships 200) (Growth 50))
          , (PlanetId 2, Planet Neutral         (Ships 150) (Growth 10))
          , (PlanetId 3, Planet Neutral         (Ships 30)  (Growth 5))
          , (PlanetId 4, Planet Neutral         (Ships 100) (Growth 20))
          ]
        wormholes = M.fromList
          [ (WormholeId 0, Wormhole homePlanet (Target 1) (Turns 1))
          , (WormholeId 1, Wormhole homePlanet (Target 2) (Turns 1))
          , (WormholeId 2, Wormhole homePlanet (Target 3) (Turns 1))
          , (WormholeId 3, Wormhole homePlanet (Target 4) (Turns 1))
          ]
          where homePlanet = Source 0
        fleets = []

example2 :: [(String, String, Integer)]
example2 = [("s","t",10), ("s","y",5), ("t","x",1),
            ("t","y",2),  ("y","t",3), ("y","x", 9),
            ("x","z",4),  ("z","x",6), ("y","z",2),
            ("z","s",7)]

example3 :: GameState
example3 = GameState planets wormholes fleets
  where planets = M.fromList
          [ (PlanetId 0, Planet (Owned Player1) (Ships 300) (Growth 0))
          , (PlanetId 1, Planet Neutral         (Ships 200) (Growth 50))
          , (PlanetId 2, Planet Neutral         (Ships 150) (Growth 10))
          , (PlanetId 3, Planet Neutral         (Ships 30)  (Growth 5))
          , (PlanetId 4, Planet Neutral         (Ships 100) (Growth 20))
          , (PlanetId 5, Planet Neutral         (Ships 100) (Growth 20))
          ]
        wormholes = M.fromList
          [ (WormholeId 0, Wormhole homePlanet (Target 1) (Turns 1))
          , (WormholeId 1, Wormhole homePlanet (Target 2) (Turns 2))
          , (WormholeId 2, Wormhole homePlanet (Target 3) (Turns 3))
          , (WormholeId 3, Wormhole homePlanet (Target 4) (Turns 4))
          , (WormholeId 4, Wormhole (Source 4) (Target 5) (Turns 1))
          , (WormholeId 5, Wormhole (Source 2) (Target 5) (Turns 1))
          ]
          where homePlanet = Source 0
        fleets = []

example4 :: [(String, String, Integer)]
example4 = [("a","b",1), ("a","c",1), ("a","d",1),
            ("b","a",1), ("c","a",1), ("d","a",1),
            ("c","d",1)]

example5 :: GameState
example5 = GameState planets wormholes fleets where
  planet :: Owner -> Int -> Int -> Planet
  planet o s g = Planet o (Ships s) (Growth g)
  planets = M.fromList
    [ (PlanetId 0, planet (Owned Player1) 300 7)
    , (PlanetId 1, planet Neutral 200 2)
    , (PlanetId 2, planet Neutral 150 3)
    , (PlanetId 3, planet Neutral 30  6)
    ]
  wormhole :: Int -> PlanetId -> PlanetId -> Int -> (WormholeId, Wormhole)
  wormhole w s t ts = (WormholeId w, Wormhole (Source s) (Target t) (Turns ts))
  wormholes = M.fromList
    [ wormhole 0 0 1 1, wormhole 1 0 2 1
    , wormhole 2 0 3 1, wormhole 3 1 0 1
    , wormhole 4 2 0 1, wormhole 5 3 0 1
    , wormhole 6 2 3 1
    ]
  fleets = []
