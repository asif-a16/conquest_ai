module Optimise where

import Data.Function (on)

import Data.Array
import Data.List (maximumBy)

import Game
import Util

knapsack :: forall name weight value. (Ord weight, Num weight, Ord value, Num value) => [(name, weight, value)] -> weight -> value
knapsack wvs c = maximumElse 0 [ v + knapsack wvs (c - w) | (_,w,v) <- wvs , w <= c ]

knapsack' :: forall name weight value. (Ix weight, Num weight, Ord value, Num value) => [(name, weight, value)] -> weight -> value
knapsack' wvs c = table ! c
  where table :: Array weight value
        table = tabulate (0, c) mknapsack

        mknapsack :: weight -> value
        mknapsack c = undefined -- TODO: Problem 1

knapsack'' :: forall name weight value. (Ix weight, Num weight, Ord value, Num value) => [(name, weight, value)] -> weight -> (value, [name])
knapsack'' wvs c = table ! c
  where table :: Array weight (value, [name])
        table = tabulate (0,c) mknapsack

        mknapsack :: weight -> (value, [name])
        mknapsack c = undefined -- TODO: Problem 2

bknapsack :: forall name weight value. (Ord weight, Num weight, Ord value, Num value) => [(name, weight, value)] -> weight -> (value, [name])
bknapsack = undefined -- TODO: Problem 3

bknapsack' :: forall name weight value. (Ord weight, Num weight, Ord value, Num value) => [(name, weight, value)] -> Int -> weight -> (value, [name])
bknapsack' = undefined -- TODO: Problem 4

bknapsack'' :: forall name weight value. (Ord name, Ix weight, Ord weight, Num weight, Ord value, Num value) => [(name, weight, value)] -> weight -> (value, [name])
bknapsack'' = undefined -- TODO: Problem 5

optimise :: GameState -> Source -> (Growth, [PlanetId])
optimise st s@(Source p) = bknapsack'' (targetPlanets st s) (shipsOnPlanet st p)
