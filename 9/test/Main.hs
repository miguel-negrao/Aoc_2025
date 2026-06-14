module Main (main) where

import AoC
import Data.Either (isRight, fromRight)
import qualified Data.Text.IO as TIO
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Text.Megaparsec (parse)
import Test.QuickCheck (Gen, Property, choose, forAll, listOf, (==>), shuffle)
import Numeric.Natural (Natural)
import qualified Data.Set as Set
import Data.List
import Math.Combinat.Sets (combine)
import qualified Math.Combinat.Sets as Sets
import qualified Data.Map.Strict as Map
import qualified Data.SBV as SBV

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
            Right parsed -> assertEqual "part1" 50 (part1 parsed)
    , testCase "part1 final" $ do
        input <- TIO.readFile "input"
        case parse parser "input" input of
            Left err -> assertFailure (show err)
            Right parsed -> assertEqual "part1" 4748769124 (part1 parsed)
    , testCase "solve2x2 solves Ax = b system (Z3)" $ do
        result <- SBV.prove solve2x2JustResultSolvesSystem
        assertBool (show result) (proved result)
    -- , testCase "part2 example" $ do
    --     input <- TIO.readFile "test_input"
    --     case parse parser "test_input" input of
    --         Left err -> assertFailure (show err)
    --         Right parsed -> assertEqual "part2" 25272 (part2 parsed)
    ]

proved :: SBV.ThmResult -> Bool
proved (SBV.ThmResult (SBV.Unsatisfiable _ _)) = True
proved _ = False

solve2x2JustResultSolvesSystem
    :: SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SBool
solve2x2JustResultSolvesSystem a1 b1 a2 b2 c1 c2 =
    let a = ((a1, b1), (a2, b2))
        c = (c1, c2)
        det = det2x2 a
        (x, y) = solve2x2' a c det
     in det SBV../= 0 SBV..=>
            (a1 * x + b1 * y SBV..== c1)
                SBV..&& (a2 * x + b2 * y SBV..== c2)
