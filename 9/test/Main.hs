{-# LANGUAGE DataKinds #-}

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
    , testCase "nonzero determinant implies one solution (Z3)" $ do
        result <- SBV.prove nonzeroDeterminantImpliesOneSolution
        assertBool (show result) (proved result)
    , testCase "segment intersection matches parametric SBV solution (Z3)" $ do
        result <- SBV.prove segmentIntersectionMatchesParametricSolution
        assertBool (show result) (proved result)
    , testCase "parametric segment intersection implies nonzero determinant (Z3)" $ do
        result <- SBV.prove parametricIntersectionImpliesNonzeroDeterminant
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

nonzeroDeterminantImpliesOneSolution
    :: SBV.Forall "a1" SBV.AlgReal
    -> SBV.Forall "b1" SBV.AlgReal
    -> SBV.Forall "a2" SBV.AlgReal
    -> SBV.Forall "b2" SBV.AlgReal
    -> SBV.Forall "c1" SBV.AlgReal
    -> SBV.Forall "c2" SBV.AlgReal
    -> SBV.Exists "x" SBV.AlgReal
    -> SBV.Exists "y" SBV.AlgReal
    -> SBV.Forall "otherX" SBV.AlgReal
    -> SBV.Forall "otherY" SBV.AlgReal
    -> SBV.SBool
nonzeroDeterminantImpliesOneSolution
    (SBV.Forall a1)
    (SBV.Forall b1)
    (SBV.Forall a2)
    (SBV.Forall b2)
    (SBV.Forall c1)
    (SBV.Forall c2)
    (SBV.Exists x)
    (SBV.Exists y)
    (SBV.Forall otherX)
    (SBV.Forall otherY) =
        (det2x2 ((a1, b1), (a2, b2)) SBV../= 0)
            SBV..=> (solves x y
                SBV..&& (solves otherX otherY
                    SBV..=> (otherX SBV..== x SBV..&& otherY SBV..== y)))
  where
    solves candidateX candidateY =
        a1 * candidateX + b1 * candidateY SBV..== c1
            SBV..&& (a2 * candidateX + b2 * candidateY SBV..== c2)

segmentIntersectionMatchesParametricSolution
    :: SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SBool
segmentIntersectionMatchesParametricSolution
    x1 y1 x2 y2 x3 y3 x4 y4 s t =
        (segmentsAreNonDegenerate
            SBV..&& det SBV../= 0
            SBV..&& isParametricIntersection s t)
            SBV..=> (solutionX SBV..== x1 + s * (x2 - x1)
                SBV..&& solutionY SBV..== y1 + s * (y2 - y1))
  where
    firstSegment = ((x1, y1), (x2, y2))
    secondSegment = ((x3, y3), (x4, y4))
    matrix = lineIntersectionMatrix firstSegment secondSegment
    constants = lineIntersectionConstants firstSegment secondSegment
    det = det2x2 matrix
    (solutionX, solutionY) = solve2x2' matrix constants det

    segmentsAreNonDegenerate = endpointsArePairwiseDistinct
        [(x1, y1), (x2, y2), (x3, y3), (x4, y4)]

    isParametricIntersection s t =
        s SBV..>= 0 SBV..&& s SBV..<= 1
            SBV..&& t SBV..>= 0 SBV..&& t SBV..<= 1
            SBV..&& (x1 + s * (x2 - x1) SBV..== x3 + t * (x4 - x3))
            SBV..&& (y1 + s * (y2 - y1) SBV..== y3 + t * (y4 - y3))

parametricIntersectionImpliesNonzeroDeterminant
    :: SBV.Forall "x1" SBV.AlgReal
    -> SBV.Forall "y1" SBV.AlgReal
    -> SBV.Forall "x2" SBV.AlgReal
    -> SBV.Forall "y2" SBV.AlgReal
    -> SBV.Forall "x3" SBV.AlgReal
    -> SBV.Forall "y3" SBV.AlgReal
    -> SBV.Forall "x4" SBV.AlgReal
    -> SBV.Forall "y4" SBV.AlgReal
    -> SBV.Forall "s" SBV.AlgReal
    -> SBV.Forall "t" SBV.AlgReal
    -> SBV.SBool
parametricIntersectionImpliesNonzeroDeterminant
    (SBV.Forall x1)
    (SBV.Forall y1)
    (SBV.Forall x2)
    (SBV.Forall y2)
    (SBV.Forall x3)
    (SBV.Forall y3)
    (SBV.Forall x4)
    (SBV.Forall y4)
    (SBV.Forall s)
    (SBV.Forall t) =
        (segmentsAreNonDegenerate
            SBV..&& isParametricIntersection s t
            SBV..&& SBV.quantifiedBool intersectionIsUnique)
            SBV..=> det SBV../= 0
  where
    firstSegment = ((x1, y1), (x2, y2))
    secondSegment = ((x3, y3), (x4, y4))
    det = det2x2 (lineIntersectionMatrix firstSegment secondSegment)

    segmentsAreNonDegenerate = endpointsArePairwiseDistinct
        [(x1, y1), (x2, y2), (x3, y3), (x4, y4)]

    isParametricIntersection candidateS candidateT =
        candidateS SBV..>= 0 SBV..&& candidateS SBV..<= 1
            SBV..&& candidateT SBV..>= 0 SBV..&& candidateT SBV..<= 1
            SBV..&& (x1 + candidateS * (x2 - x1)
                SBV..== x3 + candidateT * (x4 - x3))
            SBV..&& (y1 + candidateS * (y2 - y1)
                SBV..== y3 + candidateT * (y4 - y3))

    intersectionIsUnique
        :: SBV.Forall "otherS" SBV.AlgReal
        -> SBV.Forall "otherT" SBV.AlgReal
        -> SBV.SBool
    intersectionIsUnique (SBV.Forall otherS) (SBV.Forall otherT) =
        isParametricIntersection otherS otherT
            SBV..=> (otherS SBV..== s SBV..&& otherT SBV..== t)

endpointsArePairwiseDistinct :: [(SBV.SReal, SBV.SReal)] -> SBV.SBool
endpointsArePairwiseDistinct points =
    SBV.sAnd
        [ x1 SBV../= x2 SBV..|| y1 SBV../= y2
        | (index, (x1, y1)) <- zip [0 :: Int ..] points
        , (x2, y2) <- drop (index + 1) points
        ]
