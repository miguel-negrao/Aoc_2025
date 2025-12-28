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

genInterval :: Gen (Natural, Natural)
genInterval = do
    a <- chooseInteger (0, 100000)
    b <- chooseInteger (a, a + 100000)
    pure (fromInteger a, fromInteger b)

genRanges :: Gen [(Natural, Natural)]
genRanges = listOf genInterval

prop_intervalUnionsV3_nonOverlapping :: Property
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
    , testProperty "intervalUnionsV3 produces non-overlapping intervals" prop_intervalUnionsV3_nonOverlapping
    ]
