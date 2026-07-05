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
    , testCase "logicSort sorts concrete values" $
        assertEqual "sorted values" [1, 2, 3, 4]
            (logicSort [3, 1, 4, 2 :: Int])
    , testCase "logicSort sorts symbolic values (Z3)" $ do
        result <- SBV.prove symbolicLogicSortOrdersThree
        assertBool (show result) (proved result)
    , testCase "proper segment intersection uses shared implementation (Z3)" $ do
        result <- SBV.prove symbolicProperSegmentIntersectionExamples
        assertBool (show result) (proved result)
    , testCase "half-ray endpoint convention parametric completeness (Z3)" $ do
        result <- SBV.sat halfRayEndpointParametricCompletenessCounterexample
        assertBool (show result) (unsatisfiable result)
    , testCase "half-ray endpoint convention parametric soundness (Z3)" $ do
        result <- SBV.sat halfRayEndpointParametricSoundnessCounterexample
        assertBool (show result) (unsatisfiable result)
    , testCase "pointOnEdge soundness: parametric definition implies criterion (Z3)" $ do
        result <- SBV.prove pointOnEdgeParametricSoundness
        assertBool (show result) (proved result)
    , testCase "pointOnEdge completeness: criterion has a parametric witness (Z3)" $ do
        result <- SBV.prove pointOnEdgeParametricCompleteness
        assertBool (show result) (proved result)
    , testCase "pointOnLine soundness: parametric definition implies criterion (Z3)" $ do
        result <- SBV.prove pointOnLineParametricSoundness
        assertBool (show result) (proved result)
    , testCase "pointOnLine completeness: criterion has a real parametric witness (Z3)" $ do
        result <- SBV.prove pointOnLineParametricCompleteness
        assertBool (show result) (proved result)
    , testCase "segment intersection matches parametric SBV solution (Z3)" $ do
        result <- SBV.prove segmentIntersectionMatchesParametricSolution
        assertBool (show result) (proved result)
    , testCase "parametric segment intersection implies nonzero determinant (Z3)" $ do
        result <- SBV.prove parametricIntersectionImpliesNonzeroDeterminant
        assertBool (show result) (proved result)
    , testCase "proof: generic rectangle pointInPolygon (Z3)" $ do
        result <- SBV.prove symbolicPointInPolygonMatchesRectangleCheck
        assertBool (show result) (proved result)
    , testCase "proof: generic triangle pointInPolygon (Z3)" $ do
        result <- SBV.sat symbolicPointInPolygonTriangleCounterexample
        assertBool (show result) (unsatisfiable result)
    , testCase "proof: triangle polygon containment barycentric soundness (Z3)" $ do
        result <- SBV.sat polygonIsInsideOrOnTriangleSoundnessCounterexample
        assertBool (show result) (unsatisfiable result)
    , testCase "proof: triangle polygon containment barycentric completeness (Z3)" $ do
        result <- SBV.sat polygonIsInsideOrOnTriangleCompletenessCounterexample
        assertBool (show result) (unsatisfiable result)
    , testCase
        "polygon containment rejects rectangle spanning a concave notch"
        concaveNotchRectangleIsNotContained
    -- , testCase "part2 example" $ do
    --     input <- TIO.readFile "test_input"
    --     case parse parser "test_input" input of
    --         Left err -> assertFailure (show err)
    --         Right parsed -> assertEqual "part2" 24 (part2 parsed)
    -- , testCase "part2 final" $ do
    --     input <- TIO.readFile "input"
    --     case parse parser "input" input of
    --         Left err -> assertFailure (show err)
    --         Right parsed -> assertEqual "part2" 1525991432 (part2 parsed)
    ]

concaveNotchRectangleIsNotContained :: IO ()
concaveNotchRectangleIsNotContained =
    assertBool
        "the rectangle includes the empty notch and is not contained"
        (not (polygonIsInsideOrOn rectangle outerPolygon))
  where
    rectangle
        :: ([(Integer, Integer)], [((Double, Double), (Double, Double))])
    rectangle = (rectangleVertices, rectangleEdges)
    outerPolygon
        :: ([(Integer, Integer)], [((Double, Double), (Double, Double))])
    outerPolygon = (outerVertices, outerEdges)
    rectangleVertices = [(0, 0), (4, 0), (4, 4), (0, 4)]
    rectangleEdges =
        [ ((0, 0), (4, 0))
        , ((4, 0), (4, 4))
        , ((4, 4), (0, 4))
        , ((0, 4), (0, 0))
        ]
    outerVertices =
        [ (0, 0)
        , (4, 0)
        , (4, 4)
        , (3, 4)
        , (3, 1)
        , (1, 1)
        , (1, 4)
        , (0, 4)
        ]
    outerEdges =
        [ ((0, 0), (4, 0))
        , ((4, 0), (4, 4))
        , ((4, 4), (3, 4))
        , ((3, 4), (3, 1))
        , ((3, 1), (1, 1))
        , ((1, 1), (1, 4))
        , ((1, 4), (0, 4))
        , ((0, 4), (0, 0))
        ]

proved :: SBV.ThmResult -> Bool
proved (SBV.ThmResult (SBV.Unsatisfiable _ _)) = True
proved _ = False

unsatisfiable :: SBV.SatResult -> Bool
unsatisfiable (SBV.SatResult (SBV.Unsatisfiable _ _)) = True
unsatisfiable _ = False

symbolicProperSegmentIntersectionExamples :: SBV.SBool
symbolicProperSegmentIntersectionExamples =
    hasLineSegmentsIntersectAtInteriorPoint crossingA crossingB
        .&&. b_not
            (hasLineSegmentsIntersectAtInteriorPoint touchingA touchingB)
  where
    crossingA, crossingB, touchingA, touchingB :: SEdge
    crossingA = ((0, 0), (2, 2))
    crossingB = ((0, 2), (2, 0))
    touchingA = ((0, 0), (1, 1))
    touchingB = ((1, 1), (2, 0))

symbolicLogicSortOrdersThree
    :: SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SBool
symbolicLogicSortOrdersThree x y z =
    case logicSort [x, y, z] of
        [a, b, c] ->
            a .<=. b
                .&&. b .<=. c
                .&&. logicAny
                    [ sameValues (a, b, c) (x, y, z)
                    , sameValues (a, b, c) (x, z, y)
                    , sameValues (a, b, c) (y, x, z)
                    , sameValues (a, b, c) (y, z, x)
                    , sameValues (a, b, c) (z, x, y)
                    , sameValues (a, b, c) (z, y, x)
                    ]
        _ -> false
  where
    sameValues (a, b, c) (x', y', z') =
        a .==. x' .&&. b .==. y' .&&. c .==. z'

halfRayEndpointParametricCompletenessCounterexample :: SBV.Symbolic SBV.SBool
halfRayEndpointParametricCompletenessCounterexample = do
    pointX <- SBV.sReal "pointX"
    pointY <- SBV.sReal "pointY"
    edgeX1 <- SBV.sReal "edgeX1"
    edgeY1 <- SBV.sReal "edgeY1"
    edgeX2 <- SBV.sReal "edgeX2"
    edgeY2 <- SBV.sReal "edgeY2"
    segmentT <- SBV.sReal "segmentT"
    rayT <- SBV.sReal "rayT"
    let point = (pointX, pointY)
        edge = ((edgeX1, edgeY1), (edgeX2, edgeY2))
    pure $
        parametricHalfRayCrossingWithEndpointConvention
            point edge segmentT rayT
            .&&. b_not (lineSegmentIntersectsHalfRayGoingRight point edge)

halfRayEndpointParametricSoundnessCounterexample :: SBV.Symbolic SBV.SBool
halfRayEndpointParametricSoundnessCounterexample = do
    pointX <- SBV.sReal "pointX"
    pointY <- SBV.sReal "pointY"
    edgeX1 <- SBV.sReal "edgeX1"
    edgeY1 <- SBV.sReal "edgeY1"
    edgeX2 <- SBV.sReal "edgeX2"
    edgeY2 <- SBV.sReal "edgeY2"
    let point = (pointX, pointY)
        edge = ((edgeX1, edgeY1), (edgeX2, edgeY2))
        hasParametricCrossing = SBV.quantifiedBool (parametricWitness point edge)
    pure $
        lineSegmentIntersectsHalfRayGoingRight point edge
            .&&. b_not hasParametricCrossing
  where
    parametricWitness
        :: SPoint
        -> SEdge
        -> SBV.Exists "segmentT" SBV.AlgReal
        -> SBV.Exists "rayT" SBV.AlgReal
        -> SBV.SBool
    parametricWitness point edge (SBV.Exists segmentT) (SBV.Exists rayT) =
        parametricHalfRayCrossingWithEndpointConvention
            point edge segmentT rayT

-- This is the parametric intersection definition extended with the same
-- half-open endpoint convention used for polygon crossing parity.  An interior
-- witness is accepted directly.  For t = 0 or t = 1, the other endpoint must
-- be below the horizontal ray.
parametricHalfRayCrossingWithEndpointConvention
    :: SPoint
    -> SEdge
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SBool
parametricHalfRayCrossingWithEndpointConvention
    (pointX, pointY)
    ((edgeX1, edgeY1), (edgeX2, edgeY2))
    segmentT
    rayT =
        edgeY1 ./=. edgeY2
            .&&. acceptedSegmentParameter
            .&&. 0 .<. rayT
            .&&. edgeX1 + segmentT * (edgeX2 - edgeX1) .==. pointX + rayT
            .&&. edgeY1 + segmentT * (edgeY2 - edgeY1) .==. pointY
  where
    acceptedSegmentParameter =
        (0 .<. segmentT .&&. segmentT .<. 1)
            .||. (segmentT .==. 0 .&&. edgeY2 .<. pointY)
            .||. (segmentT .==. 1 .&&. edgeY1 .<. pointY)

pointOnEdgeParametricSoundness
    :: SBV.Forall "x1" SBV.AlgReal
    -> SBV.Forall "y1" SBV.AlgReal
    -> SBV.Forall "x2" SBV.AlgReal
    -> SBV.Forall "y2" SBV.AlgReal
    -> SBV.Forall "x" SBV.AlgReal
    -> SBV.Forall "y" SBV.AlgReal
    -> SBV.Forall "t" SBV.AlgReal
    -> SBV.SBool
pointOnEdgeParametricSoundness
    (SBV.Forall x1)
    (SBV.Forall y1)
    (SBV.Forall x2)
    (SBV.Forall y2)
    (SBV.Forall x)
    (SBV.Forall y)
    (SBV.Forall t) =
        parametricPointOnEdge x1 y1 x2 y2 x y t
            .=>. pointOnEdge ((x1, y1), (x2, y2)) (x, y)

pointOnEdgeParametricCompleteness
    :: SBV.Forall "x1" SBV.AlgReal
    -> SBV.Forall "y1" SBV.AlgReal
    -> SBV.Forall "x2" SBV.AlgReal
    -> SBV.Forall "y2" SBV.AlgReal
    -> SBV.Forall "x" SBV.AlgReal
    -> SBV.Forall "y" SBV.AlgReal
    -> SBV.Exists "t" SBV.AlgReal
    -> SBV.SBool
pointOnEdgeParametricCompleteness
    (SBV.Forall x1)
    (SBV.Forall y1)
    (SBV.Forall x2)
    (SBV.Forall y2)
    (SBV.Forall x)
    (SBV.Forall y)
    (SBV.Exists t) =
        pointOnEdge ((x1, y1), (x2, y2)) (x, y)
            .=>. parametricPointOnEdge x1 y1 x2 y2 x y t

parametricPointOnEdge
    :: SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SBool
parametricPointOnEdge x1 y1 x2 y2 x y t =
    0 .<=. t
        .&&. t .<=. 1
        .&&. x .==. x1 + t * (x2 - x1)
        .&&. y .==. y1 + t * (y2 - y1)

pointOnLineParametricSoundness
    :: SBV.Forall "x1" SBV.AlgReal
    -> SBV.Forall "y1" SBV.AlgReal
    -> SBV.Forall "x2" SBV.AlgReal
    -> SBV.Forall "y2" SBV.AlgReal
    -> SBV.Forall "x" SBV.AlgReal
    -> SBV.Forall "y" SBV.AlgReal
    -> SBV.Forall "t" SBV.AlgReal
    -> SBV.SBool
pointOnLineParametricSoundness
    (SBV.Forall x1)
    (SBV.Forall y1)
    (SBV.Forall x2)
    (SBV.Forall y2)
    (SBV.Forall x)
    (SBV.Forall y)
    (SBV.Forall t) =
        parametricPointOnLine x1 y1 x2 y2 x y t
            .=>. pointOnLine ((x1, y1), (x2, y2)) (x, y)

pointOnLineParametricCompleteness
    :: SBV.Forall "x1" SBV.AlgReal
    -> SBV.Forall "y1" SBV.AlgReal
    -> SBV.Forall "x2" SBV.AlgReal
    -> SBV.Forall "y2" SBV.AlgReal
    -> SBV.Forall "x" SBV.AlgReal
    -> SBV.Forall "y" SBV.AlgReal
    -> SBV.Exists "t" SBV.AlgReal
    -> SBV.SBool
pointOnLineParametricCompleteness
    (SBV.Forall x1)
    (SBV.Forall y1)
    (SBV.Forall x2)
    (SBV.Forall y2)
    (SBV.Forall x)
    (SBV.Forall y)
    (SBV.Exists t) =
        (x1 ./=. x2 .||. y1 ./=. y2)
            .=>. (pointOnLine ((x1, y1), (x2, y2)) (x, y)
                .=>. parametricPointOnLine x1 y1 x2 y2 x y t)

parametricPointOnLine
    :: SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SReal
    -> SBV.SBool
parametricPointOnLine x1 y1 x2 y2 x y t =
    x .==. x1 + t * (x2 - x1)
        .&&. y .==. y1 + t * (y2 - y1)

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
        (hasUniqueSolution, (x, y)) = solve2x2 a c
     in hasUniqueSolution .=>.
            ((a1 * x + b1 * y) .==. c1
                .&&. (a2 * x + b2 * y) .==. c2)

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

type SPoint = (SBV.SReal, SBV.SReal)

type SEdge = (SPoint, SPoint)

-- Created by ChatGPT
sPointSum :: SPoint -> SPoint -> SPoint
sPointSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Created by ChatGPT
sPointNegate :: SPoint -> SPoint
sPointNegate (x, y) = (-x, -y)

-- Created by ChatGPT
sPointDiff :: SPoint -> SPoint -> SPoint
sPointDiff p1 p2 = sPointSum p1 (sPointNegate p2)

-- Created by ChatGPT
sPointOnLineSegment :: SEdge -> SPoint -> SBV.SBool
sPointOnLineSegment edge@(p1, p2) point =
    det2x2 (sPointDiff p2 p1, sPointDiff point p1) SBV..== 0
        SBV..&& isInRect edge point

-- Created by ChatGPT
sPointOnAnyEdge :: [SEdge] -> SPoint -> SBV.SBool
sPointOnAnyEdge edges point = SBV.sOr (map (`sPointOnLineSegment` point) edges)

-- Created by ChatGPT
symbolicPointInPolygonMatchesRectangleCheck
    :: SBV.Forall "rectangleX1" SBV.AlgReal
    -> SBV.Forall "rectangleY1" SBV.AlgReal
    -> SBV.Forall "rectangleX2" SBV.AlgReal
    -> SBV.Forall "rectangleY2" SBV.AlgReal
    -> SBV.Forall "pointX" SBV.AlgReal
    -> SBV.Forall "pointY" SBV.AlgReal
    -> SBV.SBool
symbolicPointInPolygonMatchesRectangleCheck
    (SBV.Forall x1)
    (SBV.Forall y1)
    (SBV.Forall x2)
    (SBV.Forall y2)
    (SBV.Forall x)
    (SBV.Forall y) =
    (x1 SBV../= x2 SBV..&& y1 SBV../= y2 
        --SBV..&& SBV.sNot (sPointOnAnyEdge rectangleEdges point))
    )
        SBV..=> (pointInPolygon rectangleEdges point SBV..<=> isInRect ((x1, y1), (x2, y2)) point)
  where
    point = (x, y)
    rectangleEdges =
        [ ((x1, y1), (x2, y1))
        , ((x2, y1), (x2, y2))
        , ((x2, y2), (x1, y2))
        , ((x1, y2), (x1, y1))
        ]

-- Created by ChatGPT
sPointInTriangleByBarycentricCoordinates :: SPoint -> SPoint -> SPoint -> SPoint -> SBV.SBool
sPointInTriangleByBarycentricCoordinates (x1, y1) (x2, y2) (x3, y3) (x, y) =
    denom SBV../= 0
        SBV..&& alpha SBV..>= 0
        SBV..&& beta SBV..>= 0
        SBV..&& gamma SBV..>= 0
        SBV..&& alpha SBV..<= 1
        SBV..&& beta SBV..<= 1
        SBV..&& gamma SBV..<= 1
  where
    denom = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
    alpha = ((y2 - y3) * (x - x3) + (x3 - x2) * (y - y3)) / denom
    beta = ((y3 - y1) * (x - x3) + (x1 - x3) * (y - y3)) / denom
    gamma = 1 - alpha - beta

-- Created by ChatGPT
symbolicPointInPolygonTriangleCounterexample :: SBV.Symbolic SBV.SBool
symbolicPointInPolygonTriangleCounterexample = do
    x1 <- SBV.sReal "triangleX1"
    y1 <- SBV.sReal "triangleY1"
    x2 <- SBV.sReal "triangleX2"
    y2 <- SBV.sReal "triangleY2"
    x3 <- SBV.sReal "triangleX3"
    y3 <- SBV.sReal "triangleY3"
    x <- SBV.sReal "pointX"
    y <- SBV.sReal "pointY"
    let point@(_, pointY) = (x, y)
        p1 = (x1, y1)
        p2 = (x2, y2)
        p3 = (x3, y3)
        triangleEdges = [(p1, p2), (p2, p3), (p3, p1)]
        triangleDet = det2x2 (sPointDiff p2 p1, sPointDiff p3 p1)
        barycentricCheck =
            sPointInTriangleByBarycentricCoordinates p1 p2 p3 point
        assumptions =
            triangleDet SBV../= 0
                SBV..&& pointY SBV../= y1
                SBV..&& pointY SBV../= y2
                SBV..&& pointY SBV../= y3
    pure $
        assumptions
            SBV..&& SBV.sNot
                (pointInPolygon triangleEdges point SBV..<=> barycentricCheck)

polygonIsInsideOrOnTriangleSoundnessCounterexample :: SBV.Symbolic SBV.SBool
polygonIsInsideOrOnTriangleSoundnessCounterexample =
    polygonIsInsideOrOnTriangleCounterexample $ \implementationCheck barycentricCheck ->
        implementationCheck SBV..&& SBV.sNot barycentricCheck

polygonIsInsideOrOnTriangleCompletenessCounterexample :: SBV.Symbolic SBV.SBool
polygonIsInsideOrOnTriangleCompletenessCounterexample =
    polygonIsInsideOrOnTriangleCounterexample $ \implementationCheck barycentricCheck ->
        barycentricCheck SBV..&& SBV.sNot implementationCheck

polygonIsInsideOrOnTriangleCounterexample
    :: (SBV.SBool -> SBV.SBool -> SBV.SBool)
    -> SBV.Symbolic SBV.SBool
polygonIsInsideOrOnTriangleCounterexample disagreement = do
    ax1 <- SBV.sReal "innerTriangleX1"
    ay1 <- SBV.sReal "innerTriangleY1"
    ax2 <- SBV.sReal "innerTriangleX2"
    ay2 <- SBV.sReal "innerTriangleY2"
    ax3 <- SBV.sReal "innerTriangleX3"
    ay3 <- SBV.sReal "innerTriangleY3"
    bx1 <- SBV.sReal "outerTriangleX1"
    by1 <- SBV.sReal "outerTriangleY1"
    bx2 <- SBV.sReal "outerTriangleX2"
    by2 <- SBV.sReal "outerTriangleY2"
    bx3 <- SBV.sReal "outerTriangleX3"
    by3 <- SBV.sReal "outerTriangleY3"
    let a1 = (ax1, ay1)
        a2 = (ax2, ay2)
        a3 = (ax3, ay3)
        b1 = (bx1, by1)
        b2 = (bx2, by2)
        b3 = (bx3, by3)
        verticesA = [a1, a2, a3]
        verticesB = [b1, b2, b3]
        edgesA = [(a1, a2), (a2, a3), (a3, a1)]
        edgesB = [(b1, b2), (b2, b3), (b3, b1)]
        triangleA = (verticesA, edgesA)
        triangleB = (verticesB, edgesB)
        implementationCheck = symbolicPolygonIsInsideOrOn triangleA triangleB
        barycentricCheck = SBV.sAnd
            [ sPointInTriangleByBarycentricCoordinates b1 b2 b3 vertex
            | vertex <- verticesA
            ]
        outerTriangleDet = det2x2 (sPointDiff b2 b1, sPointDiff b3 b1)
    pure $
        outerTriangleDet SBV../= 0
            SBV..&& disagreement implementationCheck barycentricCheck
