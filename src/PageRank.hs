{-# LANGUAGE DerivingStrategies, GeneralisedNewtypeDeriving #-}
module PageRank where

import Graph
import Data.Map (Map)
import qualified Data.Map as M

import Text.Printf (printf)

newtype PageRank = PageRank Double
  deriving (Num, Eq, Ord, Fractional)
  deriving newtype (Read)

type PageRanks pageId = Map pageId PageRank

instance Show PageRank where
  show (PageRank p) = printf "%.4f" p

initPageRanks :: (Graph g e pageId, Ord pageId)
              => g -> PageRanks pageId
initPageRanks g = M.fromList [ (p, PageRank (1 / fromIntegral n)) | p <- ps ]
  where ps = vertices g
        n  = length ps

initPageRank :: Map pageId a -> PageRanks pageId
initPageRank pageId = M.map (const (PageRank (1 / fromIntegral n))) pageId
  where
    n = M.size pageId

nextPageRank :: (Ord pageId, Edge e pageId, Graph g e pageId) => g -> PageRanks pageId -> pageId -> PageRank
nextPageRank g pr i = undefined -- TODO: Problem 6
  where
    d = 0.85

nextPageRanks :: (Ord pageId, Graph g e pageId) => g -> PageRanks pageId -> PageRanks pageId
nextPageRanks g pr = M.mapWithKey (const . nextPageRank g pr) pr

pageRanks :: (Ord pageId, Graph g e pageId) => g -> [PageRanks pageId]
pageRanks g = iterate (nextPageRanks g) (initPageRanks g)

pageRank :: (Ord pageId, Graph g e pageId) => g -> PageRanks pageId
pageRank g = pageRanks g !! 200

nextPageRank' :: (Ord pageId, Edge e pageId, Graph g e pageId)
              => g -> PageRanks pageId -> PageRank -> pageId -> PageRank -> Maybe PageRank
nextPageRank' g pr k i pri
  | abs (pri - pri') < k  = Nothing
  | otherwise             = Just pri'
  where pri' = nextPageRank g pr i

nextPageRanks' :: (Ord pageId, Graph g e pageId) => g -> PageRank -> PageRanks pageId -> Maybe (PageRanks pageId)
nextPageRanks' g k pr =
  case M.mapAccumWithKey nextPageRank'' True pr of
    (True,  pr)  -> Nothing
    (False, pr') -> Just pr'
  where nextPageRank'' converged i pri =
          case nextPageRank' g pr k i pri of
            Nothing   -> (converged, pri)
            Just pri' -> (False, pri')

pageRanks' :: (Ord pageId, Graph g e pageId) => g -> PageRank -> [PageRanks pageId]
pageRanks' g k = iterateMaybe (nextPageRanks' g k) (initPageRanks g)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : maybe [] (iterateMaybe f) (f x)

pageRank' :: (Ord pageId, Graph g e pageId) => g -> PageRanks pageId
pageRank' g = undefined -- TODO: Problem 7
  where k = 0.0001
