module Main (main) where

import AoC
import Data.Either (isRight, fromRight)
import qualified Data.Text.IO as TIO
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Text.Megaparsec (parse)
import Test.QuickCheck (Gen, Property, chooseInteger, counterexample, forAll, listOf)
import Numeric.Natural (Natural)
import qualified Data.Set as Set
import Data.List
import Math.Combinat.Sets (combine, choose)


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
            Right parsed -> assertEqual "part1" 40 (part1Test parsed)
    , testCase "part1 num elems in groups and in pairs is the same" $ do
        input <- TIO.readFile "input"
        case parse parser "test_input" input of
            Left err -> assertFailure (show err)
            Right parsed -> assertEqual "part1" (sum (length <$> part1Groups 1000 parsed)) (length $ nub $ concatMap (\(a,b) -> [a,b] )$ take 1000 $ getPairsOrderedByDistance $ parsed)
    , testCase "part1 groups are disjoint" $ do
        input <- TIO.readFile "input"
        case parse parser "test_input" input of
            Left err -> assertFailure (show err)
            Right parsed -> assertBool "part1" $ all (\[a,b] -> Set.intersection a b == Set.empty) $ choose 2 $ part1Groups 1000 parsed


    -- , testCase "part2 example" $ do
    --     input <- TIO.readFile "test_input"
    --     case parse parser "test_input" input of
    --         Left err -> assertFailure (show err)
    --         Right parsed -> assertEqual "part2" 3263827 (part2 input parsed)
    ]
