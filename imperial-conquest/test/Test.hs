{-# LANGUAGE TypeApplications #-}

import ShortestPaths
import Optimise
import Game
import Graph
import PQueue
import AdjList
import Heap
import Test.Tasty
import Test.Tasty.HUnit

import Data.Map (fromList)
import qualified Data.Map as M
import Data.List
import Data.Function

main :: IO ()
main = do
  qn1Cases <- takeCasesFromFile @([(String, Int, Int)], Int) @Int "test/cases/knapsackTests.txt"
  qn2Cases <- takeCasesFromFile @([(String, Int, Int)], Int) @(Int, [String]) "test/cases/knapsackTests2.txt"
  qn3Cases <- takeCasesFromFile @([(String, Int, Int)], Int) @(Int, [String]) "test/cases/bknapsackTests.txt"
  qn4Cases <- takeCasesFromFile @([(String, Int, Int)], Int, Int) @(Int, [String]) "test/cases/bknapsackTests2.txt"
  qn5Cases <- takeCasesFromFile @([(String, Int, Int)], Int) @(Int, [String]) "test/cases/bknapsackTests3.txt"
  qn6Cases <- takeCasesFromFile @([(String, String, Integer)], String) @[Path (String, String, Integer)] "test/cases/dijkstra.txt"
  qn7Cases <- takeCasesFromFile @([(String, String, Integer)], String) @[Path (String, String, Integer)] "test/cases/dijkstra.txt"
  qn8aCases <- takeCasesFromFile @[Int] @[Int] "test/cases/insert.txt"
  qn8bCases <- takeCasesFromFile @[Int] @Int "test/cases/rank.txt"
  qn10Cases <- takeCasesFromFile @(GameState, PlanetId, PlanetId) @([PlanetId], [PlanetId], [PlanetId]) "test/cases/conflictZonesTests.txt"
  defaultMain $ testGroup "cw1"
    [ cases (qn 1) (uncurry knapsack') (const (@?=)) qn1Cases
    , cases (qn 2) (uncurry knapsack'') (checkKnapsack True) qn2Cases
    , cases (qn 3) (uncurry bknapsack) (checkKnapsack False) qn3Cases
    , cases (qn 4) (uncurry3 bknapsack') checkKnapsack' qn4Cases
    , cases (qn 5) (uncurry bknapsack'') (checkKnapsack False) qn5Cases
    , cases (qn 6) (uncurry shortestPaths) (const cmpShortestPaths) qn6Cases
    , cases (qn 7) (uncurry shortestPaths') (const cmpShortestPaths) qn7Cases
    , testGroup (qn 8) $
        [ cases "insert" (fromPQueue . toPQueue @Heap compare) (const (@?=)) qn8aCases
        , cases "rank" (rankHeap . toPQueue @Heap compare) (const (@?=)) qn8bCases
        ]
    , testGroup (qn 9) $
        [ cases "vertices" vertices (const ((@?=) `on` sort)) [adjList :=> ["a", "b", "c", "d"]]
        , cases "edges" edges (const ((@?=) `on` sort)) [adjList :=> [("a","b",10),("a","c",20),("b","a",5),("b","d",8),("d","b",3),("d","a",4)]]
        , cases "edgesFrom" (uncurry edgesFrom) (const ((@?=) `on` sort)) [(adjList, "a") :=> [("a","b",10),("a","c",20)], (adjList, "b") :=> [("b","a",5),("b","d",8)]]
        , cases "edgesTo" (uncurry edgesTo) (const ((@?=) `on` sort)) [(adjList, "a") :=> [("b","a",5),("d","a",4)], (adjList, "b") :=> [("a","b",10),("d","b",3)]]
        , cases "velem" (uncurry velem) (const (@?=)) [("x", adjList) :=> False, ("a", adjList) :=> True]
        , cases "eelem" (uncurry eelem) (const (@?=)) [(("a", "b", 3), adjList) :=> False, (("b", "d", 8), adjList) :=> True]
        ]
    , cases (qn 10) (uncurry3 conflictZones) (const ((@?=) `on` (\(a, b, c) -> (sort a, sort b, sort c)))) qn10Cases
    ]

uncurry3 :: (a->b->c -> d) -> (a,b,c) -> d
uncurry3 f ~(a, b, c) = f a b c

-- To check a solution of the Knapsack problem, we verify its weight is equal to the standard
-- answer and that it is indeed a valid selection.
checkKnapsack :: HasCallStack => Bool -> ([(String, Int, Int)], Int) -> (Int, [String]) -> (Int, [String]) -> Assertion
checkKnapsack unbounded (items, w) (expVal, _) (actVal, actItems) = do
  expVal @?= actVal
  validSelection @? "invalid knapsack selection"
  where itemsMap :: M.Map String (Int, Int)
        itemsMap = fromList (map (\(n, w, v) -> (n, (w, v))) items)

        validSelection :: Bool
        validSelection = correctWV && (unbounded || distinctItems)

        correctWV :: Bool
        correctWV = any (\wvs -> let (ws, vs) = unzip wvs in sum ws <= w && sum vs == actVal)
                        (traverse (flip M.lookup itemsMap) actItems)

        distinctItems :: Bool
        distinctItems = all isSingle (group (sort actItems))

        isSingle :: [a] -> Bool
        isSingle [_] = True
        isSingle _ = False

-- The following is a special version for checking knapsack'.
checkKnapsack' :: HasCallStack => ([(String, Int, Int)], Int, Int) -> (Int, [String]) -> (Int, [String]) -> Assertion
checkKnapsack' = checkKnapsack False . (\(a,b,c) -> (a,c))

adjList :: AdjList (String, String, Integer) String
adjList = AdjList [ ("a", [("a", "b", 10), ("a", "c", 20)])
                  , ("b", [("b", "a", 5), ("b", "d", 8)])
                  , ("c", [])
                  , ("d", [("d", "b", 3), ("d", "a", 4)])
                  ]

cmpShortestPaths :: HasCallStack => Edge e v => [Path e] -> [Path e] -> Assertion
cmpShortestPaths qs ps = contains ps qs && contains qs ps @? "both lists must contain the same paths"
  where contains qs ps = all (\p -> any (sameAs p) qs) ps
        sameAs q p = source p == source q && target p == target q && weight p == weight q

-- Test Utils:
data Test input result = input :=> result deriving (Show, Read)

takeCasesFromFile :: (Read i, Read r) => FilePath -> IO [Test i r]
takeCasesFromFile file = map read . lines <$> readFile file

doTest :: (i -> r) -> (i -> r -> r -> Assertion) -> Test i r -> Assertion
doTest f cmp (input :=> expected) =
  let actual = f input
  in cmp input actual expected

numberedTests :: [Assertion] -> [TestTree]
numberedTests = zipWith (\n -> testCase ("#" ++ show n)) ([1..] :: [Integer])

qn :: Int -> String
qn i = "Q" ++ show i

cases :: String -> (i -> r) -> (i -> r -> r -> Assertion) -> [Test i r] -> TestTree
cases name f cmp = testGroup name . numberedTests . map (doTest f cmp)
