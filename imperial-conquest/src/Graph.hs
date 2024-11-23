{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graph where

import Data.List (nub, foldl')
import GHC.Generics (Generic)

type Weight = Integer
data Path e = Path Weight [e] deriving (Eq, Read, Show, Generic)

class Eq v => Edge e v | e -> v where
  source :: e -> v
  target :: e -> v
  weight :: e -> Weight

class Edge e v => Graph g e v | g -> e where
  vertices  :: g -> [v]
  edges     :: g -> [e]
  edgesFrom :: g -> v -> [e]
  edgesTo   :: g -> v -> [e]
  velem     :: v -> g -> Bool
  eelem     :: e -> g -> Bool

pathFromEdge :: Edge e v => e -> Path e
pathFromEdge e = Path (weight e) [e]

extend :: Edge e v => Path e -> e -> Path e
extend (Path _ []) _ = error "extend: Empty path"
extend (Path d (e:es)) e'
  | target e == source e' = Path (d + weight e') (e':e:es)
  | otherwise = error "extend: Incompatible endpoints"

pathFromEdges :: Edge e v => [e] -> Path e
pathFromEdges (x : xs) = foldl' extend (pathFromEdge x) xs
pathFromEdges [] = error "pathFromEdges: Empty list of edges"

cmpPath :: Path v -> Path v -> Ordering
cmpPath (Path d _) (Path d' _) = compare d d'

instance Eq v => Edge (v, v, Weight) v where
  source (s, _, _) = s
  target (_, t, _) = t
  weight (_, _, i) = i

instance (Eq e, Edge e v) => Graph [e] e v where
  vertices es = nub (map source es ++ map target es)
  edges es    = es
  edgesFrom es v = [ e | e <- es, v == source e ]
  edgesTo   es v = [ e | e <- es, v == target e ]
  velem v es = v `elem` vertices es
  eelem v es = v `elem` edges es

instance Edge e v => Edge (Path e) v where
  source (Path _ es) = source (last es)
  target (Path _ es) = target (head es)
  weight (Path w _)  = w
