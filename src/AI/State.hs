module AI.State where

import Game
import Data.Map (Map)

import AI.PlanetRank

data State = State
  { turn :: Turns
  , rushTarget :: Maybe PlanetId
  , prs :: Maybe PlanetRanks
  }

initialState :: State
initialState = State
  { turn = 0
  , rushTarget = Nothing
  , prs = Nothing
  }
