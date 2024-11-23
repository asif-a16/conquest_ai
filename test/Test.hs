import ShortestPaths
import Game
import Graph
import PageRank
import AI.State as AI
import AI.PlanetRank
import AI.Strategy

import Test.Tasty
import Test.Tasty.HUnit

import Data.Map (fromList)
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Function

import Control.Monad

main :: IO ()
main = do
  qn1Cases <- takeCasesFromFile @GameState @(Maybe PlanetId) "test/cases/findEnemyTests.txt"
  qn2Cases <- takeCasesFromFile @(WormholeId, Maybe Ships, GameState) @[Order] "test/cases/sendTests.txt"
  qn3Cases <- takeCasesFromFile @(PlanetId, GameState) @[Order] "test/cases/attackFromAllTests.txt"
  qn4Cases <- takeCasesFromFile @(GameState, Maybe PlanetId) @([Order], Maybe PlanetId) "test/cases/zergRush.txt"
  qn6Cases <- takeCasesFromFile @(GameState, PageRanks PlanetId, PlanetId) @PageRank "test/cases/nextPageRank.txt"
  qn7Cases <- takeCasesFromFile @GameState @(PageRanks PlanetId) "test/cases/pageRankTests.txt"
  qn8Cases <- takeCasesFromFile @(GameState, PlanetRanks, PlanetId) @PlanetRank "test/cases/nextPlanetRank.txt"
  qn9Cases <- takeCasesFromFile @GameState @[Order] "test/cases/planetRankRush.txt"
  qn10Cases <- takeCasesFromFile @GameState @[Order] "test/cases/timidRush.txt"
  defaultMain $ testGroup "cw2"
    [ cases (qn 1) findEnemyPlanet (const (@?=)) qn1Cases
    , cases (qn 2) (uncurry3 send) (const sameOrders) qn2Cases
    , cases (qn 3) (uncurry attackFromAll) checkAttackFromAll qn3Cases
    , cases (qn 4) (rushWrapper zergRush) checkZergRush qn4Cases
    , cases (qn 5) (initPageRank @Int @Int) (const same) initRankTests
    , cases (qn 6) (uncurry3 (nextPageRank @PlanetId @_ @GameState)) (const same') qn6Cases
    , cases (qn 7) (pageRank' @PlanetId @GameState) (const same) qn7Cases
    , cases (qn 8) (uncurry3 nextPlanetRank) (const same') qn8Cases
    , cases (qn 8) planetRankRushWrapper checkPlanetRankRush qn9Cases
    , cases (qn 10) timidRushWrapper timidRushCmp qn10Cases
    ]

uncurry3 :: (a->b->c -> d) -> (a,b,c) -> d
uncurry3 f ~(a, b, c) = f a b c

same' :: (Ord a, Fractional a, Show a, HasCallStack) => a -> a -> Assertion
same' a b = close (abs (a - b))

same :: (Ord k, Fractional a, Ord a, Show a, HasCallStack) => M.Map k a -> M.Map k a -> Assertion
same ps1 ps2 = forM_ (M.unionWith (\p1 p2 -> abs (p1 - p2)) ps1 ps2) close

sameOrders :: HasCallStack => [Order] -> [Order] -> Assertion
sameOrders = (@?=) `on` normalise

normalise :: [Order] -> [Order]
normalise os = map (foldr1 combine) (groupBy ((==) `on` wh) (sortOn wh os))
  where
    wh (Order w _) = w

    combine :: Order -> Order -> Order
    combine (Order w s) (Order _ s') = Order w (s + s')

checkAttackFromAll :: HasCallStack => (PlanetId, GameState) -> [Order] -> [Order] -> Assertion
checkAttackFromAll (tgtId, gs@(GameState ps ws fs)) actualOrders _ =
  do forM_ orders onShortestPath
     allSent
  where orders :: [Order]
        orders = normalise actualOrders

        -- By computing the shortest paths on the dual graph from the target, we know
        -- the shortest distance from each planet to the target planet.
        distanceToTarget :: M.Map PlanetId Integer
        distanceToTarget = M.fromList $ (tgtId, 0) : map (\p -> (target p, weight p)) (shortestPaths (dualGameState gs) tgtId)

        onShortestPath :: Order -> Assertion
        onShortestPath (Order wId ships) = assertBool "not on shortest path" $ isJust $
          do (Wormhole (Source s) (Target t) (Turns turns)) <- M.lookup wId ws
             p@(Planet _ totalShips _) <- M.lookup s ps
             guard (ourPlanet p)
             guard (totalShips == ships)
             tToTarget <- M.lookup t distanceToTarget
             sToTarget <- M.lookup s distanceToTarget
             guard (toInteger turns + tToTarget == sToTarget)

        allSent :: Assertion
        allSent = forM_ ourPlanets (\p -> assertBool "all ships should be sent" $ any (fromPlanet p) orders)
          where
            ourPlanets :: [PlanetId]
            ourPlanets = filter (\pId -> ourPlanet (lookupPlanet pId gs)) (M.keys ps)

            fromPlanet :: PlanetId -> Order -> Bool
            fromPlanet pId (Order wId _) = fromMaybe False $
              do (Wormhole (Source s) _ _) <- M.lookup wId ws
                 return (s == pId)

-- A game state with all wormholes' source and target flipped.
dualGameState :: GameState -> GameState
dualGameState (GameState ps ws fs) = GameState ps wsOp fs where
  wsOp = M.map (\(Wormhole (Source s) (Target t) turns) -> Wormhole (Source t) (Target s) turns) ws

rushWrapper :: (GameState -> AI.State -> ([Order], [String], AI.State))
            -> ((GameState, Maybe PlanetId) -> ([Order], Maybe PlanetId))
rushWrapper f (gs, t) = let (os, _, as) = f gs (initialState {rushTarget = t})
                        in (os, rushTarget as)

checkZergRush :: (GameState, Maybe PlanetId) -> ([Order], Maybe PlanetId) -> ([Order], Maybe PlanetId) -> Assertion
checkZergRush (gs, p) (osAct, pAct) (osExp, pExp) = do
  pAct @?= pExp
  null osAct @?= null osExp

initRankTests :: [Test (M.Map Int Int) (M.Map Int PageRank)]
initRankTests = [ M.fromList [(0, 0), (1,0), (2,0), (3,0)] :=> M.fromList [(0, 0.25), (1, 0.25), (2, 0.25), (3, 0.25)]
                , M.fromList [(0, 0), (1,0)] :=> M.fromList [(0, 0.5), (1, 0.5)]
                , M.fromList [(0, 0)] :=> M.fromList [(0, 1)] ]

planetRankRushWrapper :: GameState -> [Order]
planetRankRushWrapper gs = let (o, _, _) = planetRankRush gs initialState in o

timidRushWrapper :: GameState -> [Order]
timidRushWrapper gs = let (o, _, _) = timidRush gs initialState in o

-- This is deliberately weak since a full check will be expensive and reveal the solution.
checkPlanetRankRush :: GameState -> [Order] -> [Order] -> Assertion
checkPlanetRankRush _ = (@?=) `on` null

timidRushCmp :: GameState -> [Order] -> [Order] -> Assertion
timidRushCmp _ = sameOrders

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

epsilon :: (Fractional a, Show a) => a
epsilon = 0.001

close :: forall a. (Ord a, Fractional a, Show a, HasCallStack) => a -> Assertion
close delta = delta < epsilon @? ("difference " ++ show delta ++ " is not within " ++ show (epsilon @a))
