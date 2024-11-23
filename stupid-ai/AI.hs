module Main (main) where

import Game
import AI.State (State(turn))
import AI.IO

import qualified Data.Map as M

main :: IO ()
main = game logic

logic :: StepLogic
logic st aist = (
    concatMap attackAllNeighbours (availableTargets st),
    dummyLog st,
    aist
  )

dummyLog :: GameState -> Log
dummyLog st = ["We have " ++ show (length (ourPlanets st)) ++ " planets!"]

availableTargets :: GameState -> [(Planet, Wormholes)]
availableTargets st = map (\(pId, p) -> (p, wormholesFrom (Source pId) st))
                          (M.assocs (ourPlanets st))

attackAllNeighbours :: (Planet, Wormholes) -> [Order]
attackAllNeighbours (Planet _ (Ships s) _, ws)
  | null ws   = []
  | each > 0  = map (\(wId, _) -> Order wId (Ships each)) (M.assocs ws)
  | otherwise = []
  where each = max 0 (s `div` length ws)
