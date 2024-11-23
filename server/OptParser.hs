{-# LANGUAGE BlockArguments #-}
module OptParser (parseArgs, UI (..), isTUI, Arguments (..)) where

import Data.List (intercalate)
import System.Random

import Options.Applicative

import Game
import AI.Strategy (Strategy, defaultStrategy)

data UI = Headless | TUI Int deriving (Ord, Eq, Show)

isTUI :: UI -> Bool
isTUI (TUI _) = True
isTUI _       = False

-- The datatype for command line arguments.
data Arguments = Arguments { arg_p1        :: !String
                           , arg_p2        :: !String
                           , arg_seed      :: !(Maybe StdGen)
                           , arg_strategy1 :: !Strategy
                           , arg_strategy2 :: !Strategy
                           , arg_ui        :: !UI
                           , arg_turns     :: !Turns
                           , arg_recomp    :: !Bool
                           , arg_stepping  :: !Bool
                           , arg_timeout   :: !Word
                           }

parseArgs :: IO Arguments
parseArgs = customExecParser (prefs showHelpOnError) $
  info arguments (fullDesc <> header "The server of Imperial Conquest"
                           <> progDesc "The server must take two AIs as arguments")

arguments :: Parser Arguments
arguments =
  Arguments <$> playerArg 1
            <*> playerArg 2
            <*> optional (option seedReader (long "seed" <> metavar "<seed>" <> help "random generator seed for the map"))
            <*> strategyOpt 1
            <*> strategyOpt 2
            <*> (flag' Headless (long "headless" <> help "run the game without drawing")
            <|> TUI <$> option posNumReader (value 1 <> long "draw-interval" <> metavar "<turns>" <> help "when runnning in TUI, draw the map every <turns> turns." <> showDefault))
            <*> option posNumReader (value 400 <> long "turns" <> metavar "<turns>" <> help "the total number of turns for the game" <> showDefault)
            <*> switch (long "stepping" <> help "when running in TUI, wait for a key press before showing the next turn")
            <*> switch (long "recomp" <> help "force recompilation of the AIs")
            <*> option auto (value 10 <> long "timeout" <> metavar "<seconds>" <> help "the timeout for each AI" <> showDefault)

--TODO: really, this should be limited to only valid cabal package ids
playerArg :: Int -> Parser String
playerArg n = strArgument (metavar ("bot" ++ show n) <> help ("the cabal package for player" ++ show n))

strategyOpt :: Int -> Parser Strategy
strategyOpt n = option auto
  (value defaultStrategy <> long ("strategy" ++ show n)
                         <> metavar ("[ " ++ intercalate " | " (map show strats) ++ " ]")
                         <> help ("strategy for player " ++ show n) <> showDefault)
  where strats = [minBound @Strategy .. maxBound @Strategy]

seedReader :: ReadM StdGen
seedReader = eitherReader \s ->
  case reads (map dashToSpace s) of
    [(a, "")] -> Right a
    _         -> Left ("Cannot parse seed" ++ s)
  where dashToSpace '-' = ' '
        dashToSpace c = c

posNumReader :: Num a => ReadM a
posNumReader = fromIntegral <$> auto @Word
