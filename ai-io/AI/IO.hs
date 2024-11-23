module AI.IO (game, gameM, StepLogic, GameMonad, Log) where

import Game
import AI.State as AI (State(turn), initialState)

import System.IO
import Debug.Trace
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.ByteString as BS

import Control.Monad
import Control.Monad.State

type Log = [String]
type GameMonad = StateT AI.State IO ([Order], Log)
type StepLogic = GameState -> AI.State -> ([Order], Log, AI.State)

liftGame :: StepLogic -> GameState -> GameMonad
liftGame l st = StateT $ \aist ->
  let (orders, log, aist') = l st aist
  in return ((orders, log), aist')

game :: StepLogic -> IO ()
game = gameM . liftGame

-- You might want to use this if your skynet were to need IO!
gameM :: (GameState -> GameMonad) -> IO ()
gameM logic = do
  hSetBinaryMode stdout True
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  contents <- incrementalExample <$> B.getContents
  flip evalStateT initialState $ forM_ contents $ \ms -> do
    t <- gets turn
    modify (\aist -> aist { turn = turn aist + 1 })
    (orders, logs) <- logic ms
    liftIO $ unless (null logs) $ do
      traceIO ("-------------- " ++ show t ++ ": --------------")
      traceIO (unlines logs)
    let payload = B.encode orders
    liftIO (B.putStr (B.encode (B.length payload)))
    liftIO (B.putStr payload)

incrementalExample :: B.ByteString -> [GameState]
incrementalExample = go decoder
  where
    decoder = B.runGetIncremental B.get
    go :: B.Decoder GameState -> B.ByteString -> [GameState]
    go (B.Done leftover _ trade) input = trade : go decoder (B.chunk leftover input)
    go (B.Partial k) input = go (k . takeHeadChunk $ input) (dropHeadChunk input)
    go (B.Fail _ _ msg) _ = error msg

takeHeadChunk :: B.ByteString -> Maybe BS.ByteString
takeHeadChunk (B.Chunk bs _) = Just bs
takeHeadChunk _              = Nothing

dropHeadChunk :: B.ByteString -> B.ByteString
dropHeadChunk (B.Chunk _ lbs) = lbs
dropHeadChunk _               = B.Empty
