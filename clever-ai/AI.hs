module Main (main) where

import Game
import AI.Strategy (Strategy, strategise)
import AI.IO

import System.Environment

main :: IO ()
main = do
  [strat] <- map (read @Strategy) <$> getArgs
  game (strategise strat)
