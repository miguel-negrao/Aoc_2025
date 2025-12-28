module Main (main) where

import AoC (intervalUnionsV3, parser, part1, part2)
import Data.Either (isRight)
import qualified Data.Text.IO as TIO
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Text.Megaparsec (parse)
import Test.QuickCheck (Gen, Property, chooseInteger, counterexample, forAll, listOf)
import Numeric.Natural (Natural)
import qualified Data.Set as Set

genInterval :: Gen (Natural, Natural)
genInterval = do
    a <- chooseInteger (0, 100000)
    b <- chooseInteger (a, a + 100000)
    pure (fromInteger a, fromInteger b)

genRanges :: Gen [(Natural, Natural)]
genRanges = listOf genInterval

genSmallInterval :: Gen (Natural, Natural)
genSmallInterval = do
    a <- chooseInteger (0, 1000)
    b <- chooseInteger (a, a + 1000)
    pure (fromInteger a, fromInteger b)

genSmallRanges :: Gen [(Natural, Natural)]
genSmallRanges = listOf genSmallInterval

intervalsToSet :: [(Natural, Natural)] -> Set.Set Natural
intervalsToSet = foldMap (\(a,b) -> Set.fromList [a..b])

prop_intervalUnionsV3_nonOverlapping :: Property
-- No point should be covered by more than one merged interval.
prop_intervalUnionsV3_nonOverlapping =
    forAll genRanges $ \ranges ->
        let
            merged = intervalUnionsV3 ranges
            maxEnd = maximum (0 : fmap snd merged)
        in forAll (chooseInteger (0, toInteger maxEnd + 1)) $ \n ->
            let
                nNat = fromInteger n
                count = length [() | (a,b) <- merged, a <= nNat && nNat <= b]
            in counterexample ("merged=" <> show merged <> " n=" <> show nNat) (count <= 1)

prop_intervalUnionsV3_setEquivalent :: Property
-- The merged intervals should cover exactly the same elements as the input (small ranges).
prop_intervalUnionsV3_setEquivalent =
    forAll genSmallRanges $ \ranges ->
        let
            merged = intervalUnionsV3 ranges
        in counterexample ("ranges=" <> show ranges <> " merged=" <> show merged)
            (intervalsToSet ranges == intervalsToSet merged)

prop_intervalUnionsV3_idempotent :: Property
-- Merging an already-merged set of intervals should not change it.
prop_intervalUnionsV3_idempotent =
    forAll genRanges $ \ranges ->
        let
            merged = intervalUnionsV3 ranges
        in counterexample ("merged=" <> show merged)
            (intervalUnionsV3 merged == merged)

prop_intervalUnionsV3_validIntervals :: Property
-- All output intervals should have start <= end.
prop_intervalUnionsV3_validIntervals =
    forAll genRanges $ \ranges ->
        let
            merged = intervalUnionsV3 ranges
        in counterexample ("merged=" <> show merged)
            (all (\(a,b) -> a <= b) merged)

main :: IO ()
main = defaultMain $ testGroup "AoC5"
    [ testCase "parser parses test_input" $ do
        input <- TIO.readFile "test_input"
        assertBool "expected parse to succeed" (isRight (parse parser "test_input" input))
    , testCase "parser parses input" $ do
        input <- TIO.readFile "input"
        assertBool "expected parse to succeed" (isRight (parse parser "input" input))
    , testCase "part1 example" $ do
        input <- TIO.readFile "test_input"
        case parse parser "test_input" input of
            Left err -> assertFailure (show err)
            Right parsed -> assertEqual "part1" 3 (part1 parsed)
    , testCase "part2 example" $ do
        input <- TIO.readFile "test_input"
        case parse parser "test_input" input of
            Left err -> assertFailure (show err)
            Right parsed -> assertEqual "part2" 14 (part2 parsed)
    , testCase "intervalUnionsV3 merges touching endpoints (forward)" $ do
        assertEqual "touching endpoints" [(1,5)] (intervalUnionsV3 [(1,3),(3,5)])
    , testCase "intervalUnionsV3 merges touching endpoints (reverse)" $ do
        assertEqual "touching endpoints" [(1,5)] (intervalUnionsV3 [(3,5),(1,3)])
    , testProperty "intervalUnionsV3 produces non-overlapping intervals" prop_intervalUnionsV3_nonOverlapping
    , testProperty "intervalUnionsV3 preserves union (small ranges)" prop_intervalUnionsV3_setEquivalent
    , testProperty "intervalUnionsV3 is idempotent" prop_intervalUnionsV3_idempotent
    , testProperty "intervalUnionsV3 outputs valid intervals" prop_intervalUnionsV3_validIntervals
    ]
