module Optimise where

import Data.Function (on)
import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)
import Game
import Util

knapsack :: forall name weight value.
            (Ord weight, Num weight, Ord value, Num value)
         => [(name, weight, value)] -> weight -> value
knapsack wvs c = maximumElse 0 [ v + knapsack wvs (c - w) 
                               | (_,w,v) <- wvs , w <= c ]

knapsack' :: forall itemName itemWeight itemValue. 
             (Ix itemWeight, Num itemWeight, Ord itemValue, Num itemValue) 
          => [(itemName, itemWeight, itemValue)] -> itemWeight -> itemValue
knapsack' items maxCapacity = valueTable ! maxCapacity
  where 
    valueTable :: Array itemWeight itemValue
    valueTable = tabulate (0, maxCapacity) maxValueForCapacity

    maxValueForCapacity :: itemWeight -> itemValue
    maxValueForCapacity 0 = 0
    maxValueForCapacity currentCapacity = maximumElse 0 
      [ itemValue + valueTable ! (currentCapacity - itemWeight) 
      | (_, itemWeight, itemValue) <- items, itemWeight <= currentCapacity ]

knapsack'' :: forall name weight value. 
              (Ix weight, Num weight, Ord value, Num value) 
           => [(name, weight, value)] -> weight -> (value, [name])
knapsack'' items maxCapacity = valueTable ! maxCapacity
  where
    valueTable :: Array weight (value, [name])
    valueTable = tabulate (0, maxCapacity) maxValueForCapacity

    maxValueForCapacity :: weight -> (value, [name])
    maxValueForCapacity 0 = (0, []) 
    maxValueForCapacity currentCapacity =
      maximumBy (comparing fst) $ -- Compare by the first element of the tuple (value)
        (0, []) :                  -- Empty knapsack case
        [ (itemValue + previousValue, itemName : previousItems)
        | (itemName, itemWeight, itemValue) <- items, itemWeight <= currentCapacity,
          let (previousValue, previousItems) = valueTable ! (currentCapacity - itemWeight)
        ]

bknapsack :: forall name weight value. 
             (Ord weight, Num weight, Ord value, Num value) 
          => [(name, weight, value)] -> weight -> (value, [name])
bknapsack [] _ = (0, [])  -- Base case: no items left
bknapsack _ 0  = (0, [])   -- Base case: no capacity left
bknapsack ((name, weight, value):rest) capacity
  | weight > capacity = bknapsack rest capacity
  | otherwise =
      let
        -- Case 1: Skip the current item
        -- Calculate the maximum value without including this item
        (skipValue, skipItems) = bknapsack rest capacity
        
        -- Case 2: Take the current item (if it fits)
        (takeValue, takeItems) = bknapsack rest (capacity - weight)
      in
        -- Compare the two cases and take the better one
        if value + takeValue > skipValue
          then (value + takeValue, name : takeItems)
          else (skipValue, skipItems)

bknapsack' :: forall name weight value. 
              (Ord weight, Num weight, Ord value, Num value) 
           => [(name, weight, value)] -> Int -> weight -> (value, [name])
bknapsack' items 0 _ = (0, [])  -- Base case: no items to consider
bknapsack' items _ 0 = (0, [])  -- Base case: no capacity left
bknapsack' items i capacity =
  let
    -- The current item (1-based index)
    (name, weight, value) = items !! (i - 1)

    -- Case 1: Skip the current item
    (skipValue, skipItems) = bknapsack' items (i - 1) capacity

    -- Case 2: Take the current item (if it fits)
    (takeValue, takeItems) =
      if weight <= capacity
      then let (remainingValue, remainingItems) = bknapsack' items (i - 1) (capacity - weight)
           in (value + remainingValue, name : remainingItems)
      else (0, [])
  in
    -- Compare the two cases and choose the better one
    if takeValue > skipValue
      then (takeValue, takeItems)
      else (skipValue, skipItems)

bknapsack'' :: forall name weight value. 
               (Ord name, Ix weight, Ord weight, Num weight, Ord value, Num value) 
            => [(name, weight, value)] -> weight -> (value, [name])
bknapsack'' items maxCapacity = table ! (n, maxCapacity)
  where
    -- Number of items
    n = length items

    -- table: (Int, weight) -> (value, [name])
    table :: Array (Int, weight) (value, [name])
    table = array ((0, 0), (n, maxCapacity))
      [((i, w), bknapsackEntry i w) | i <- [0..n], w <- range (0, maxCapacity)]

    -- DP entry function
    bknapsackEntry :: Int -> weight -> (value, [name])
    bknapsackEntry 0 _ = (0, [])  -- Base case: no items to consider
    bknapsackEntry _ 0 = (0, [])  -- Base case: no capacity left
    bknapsackEntry i w =
      let
        -- Current item (1-based index for items)
        (name, weight, value) = items !! (i - 1)

        -- Case 1: Skip the current item
        (skipValue, skipItems) = table ! (i - 1, w)

        -- Case 2: Take the current item (if it fits)
        (takeValue, takeItems) =
          if weight <= w
          then let (remainingValue, remainingItems) = table ! (i - 1, w - weight)
               in (value + remainingValue, name : remainingItems)
          else (0, [])
      in
        -- Compare the two cases and choose the better one
        if takeValue > skipValue
          then (takeValue, takeItems)
          else (skipValue, skipItems)

optimise :: GameState -> Source -> (Growth, [PlanetId])
optimise st s@(Source p) = bknapsack'' (targetPlanets st s) (shipsOnPlanet st p)
