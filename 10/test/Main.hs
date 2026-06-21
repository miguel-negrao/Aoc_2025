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
            Right parsed -> assertEqual "part1" p1testanswer (part1 parsed)
    , testCase "part1 final" $ do
        input <- TIO.readFile "input"
        case parse parser "input" input of
            Left err -> assertFailure (show err)
            Right parsed -> assertEqual "part1" p1answer (part1 parsed)
    , testCase "part1v4 SBV example" $ do
         input <- TIO.readFile "test_input"
         case parse parser "test_input" input of
             Left err -> assertFailure (show err)
             Right parsed -> do
                res <- part1v4 parsed
                assertEqual "part2" (fromIntegral p1testanswer) res
    , testCase "part1v4 SBV final" $ do
         input <- TIO.readFile "input"
         case parse parser "input" input of
             Left err -> assertFailure (show err)
             Right parsed -> do
                res <- part1v4 parsed
                assertEqual "part1v4" (fromIntegral p1answer) res
    -- , testCase "part2 example" $ do
    --     input <- TIO.readFile "test_input"
    --     case parse parser "test_input" input of
    --         Left err -> assertFailure (show err)
    --         Right parsed -> assertEqual "part2v1" p2testanswer (part2 parsed)
    , testCase "part2v2 SBV example" $ do
         input <- TIO.readFile "test_input"
         case parse parser "test_input" input of
             Left err -> assertFailure (show err)
             Right parsed -> do
                res <- part2v2 parsed
                assertEqual "part2" (fromIntegral p2testanswer) res
    , testCase "part2v2 SBV final" $ do
         input <- TIO.readFile "input"
         case parse parser "input" input of
             Left err -> assertFailure (show err)
             Right parsed -> do
                res <- part2v2 parsed
                assertEqual "part2" (fromIntegral p2answer) res
    ] where
        p1testanswer = 7
        p2testanswer = 33
        p1answer = 466
        p2answer = 17214
