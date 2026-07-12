{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{- HLINT ignore "Unused LANGUAGE pragma" -}

{--
part1
time: 11m
attempts: 1
used chatgpt: no
notes: very easy, was very similar to day 8. This means part 2 will be hard!

part2
time: 2 weeks
attempts: 1
used chatgpt: yes, to research computacional geometry alrightms in general, but also wikipedia. It was used to determine the algorithmn for the function polygonIsInsideOrOn. ChatGPT as used to help create sbv proofs of correctness for my hand-coded functions.  All the code in thihs file is my own.
notes:
I decided to first research a bit the algorithms used for this, as this seemed a bit too far away from the usual CS algorithms.
The algorithm was a too tricky to find on my own because touching edges and vertices must be allowed. In the end I needed help for the algorithmn of one of the functions. The first try was correct, clocking at 4m execution time (used Rational). Will try to make it more eficient (and still correct).

times:
1st attempt: 4m
2nd attempt 2.4s calculated edges only once and changed from Rationals to Doubles (result wrong due to Float)
3rd attempt 1,0s memoization of pointInPolygon (result wrong due to Float)
4th attempt 1.5s changed float to double to have correct result

All
  part1 without parsing: OK
    39.3 ms ± 2.0 ms
  part2 without parsing: OK
    1.527 s ±  54 ms
  part1 with parsing:    OK
    41.0 ms ± 2.8 ms
  part2 with parsing:    OK
    1.508 s ±  91 ms

All 4 tests passed (228.41s)
Benchmark bench: FINISH

todo: improve speed

New approach:
algorithm
polygon is inside or on if all the edges are inside or on boundary.
--}

module AoC
    ( Parser
    , parser
    , part1
    , part2
    , ParsedType
    , det2x2
    , solve2x2'
    , solve2x2
    , lineIntersectionMatrix
    , lineIntersectionConstants
    , BooleanLogic(..)
    , LogicBoolean
    , LogicIte(..)
    , LogicEq(..)
    , LogicOrd(..)
    , logicAll
    , logicAny
    , logicSort
    , logicSortOn
    , isInInterval
    , isInRect
    , pointEq
    , hasLineSegmentsIntersectAtInteriorPoint
    , lineSegmentIntersectsHalfRayGoingRight
    , oddParity
    , pointInPolygon
    , pointOnEdge
    , pointOnLine
    , lineSegmentEdgeIntersectionPoints
    , polygonIsInsideOrOn
    ) where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace (trace)
import GHC.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Error
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO

import qualified Data.MemoCombinators as Memo
import Data.MemoCombinators (Memo)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad
import Text.Megaparsec.Debug

import Math.Combinat.Sets (combine, choose)
import Data.Ord (Down(..))
import GHC.Base (leInt, DoubleBox)
import Data.Ratio (numerator, denominator, (%))
import qualified Data.MemoCombinators.Class as Memo
import qualified Data.SBV as SBV

-- Z3 coverage: the `Z3-tested` comments below name the symbolic properties in
-- test/Main.hs that exercise each function, directly or through a tested
-- caller. Functions without such a marker currently have no Z3-based test.

-- | Boolean operations shared by concrete calculations and SBV formulas.
--
-- The methods deliberately operate only on Boolean-valued expressions. Numeric
-- equality and ordering belong in a separate abstraction over the numeric type.
infixr 3 .&&.
infixr 2 .||., .<+>.
infixr 1 .=>., .<=>.

-- Z3-tested through the symbolic geometry and sorting properties.
-- ChatGPT
class BooleanLogic b where
    true :: b
    false :: b
    b_not :: b -> b
    (.&&.) :: b -> b -> b
    (.||.) :: b -> b -> b
    (.<+>.) :: b -> b -> b
    (.=>.) :: b -> b -> b
    (.<=>.) :: b -> b -> b

-- | The Boolean type produced by comparisons on a value.
--
-- Concrete values produce 'Bool'; SBV values produce 'SBV.SBool'.  Keeping
-- this relationship in one closed family means that a constraint such as
-- @LogicOrd a@ also determines the Boolean type used inside the function.
type family LogicBoolean a where
    LogicBoolean (SBV.SBV a) = SBV.SBool
    LogicBoolean a = Bool

-- | Equality whose result can be either a concrete or symbolic Boolean.
infix 4 .==., ./=.

-- Z3-tested through the symbolic geometry and solver properties.
-- ChatGPT
class BooleanLogic (LogicBoolean a) => LogicEq a where
    (.==.) :: a -> a -> LogicBoolean a
    (./=.) :: a -> a -> LogicBoolean a
    left ./=. right = b_not (left .==. right)

-- | Ordering whose result can be either a concrete or symbolic Boolean.
infix 4 .<., .<=., .>., .>=.

-- Z3-tested through the symbolic geometry and sorting properties.
-- ChatGPT
class LogicEq a => LogicOrd a where
    (.<.) :: a -> a -> LogicBoolean a
    (.<=.) :: a -> a -> LogicBoolean a
    (.>.) :: a -> a -> LogicBoolean a
    (.>=.) :: a -> a -> LogicBoolean a

-- ChatGPT
instance BooleanLogic Bool where
    true = True
    false = False
    b_not = not
    (.&&.) = (&&)
    (.||.) = (||)
    (.<+>.) = (/=)
    antecedent .=>. consequent = not antecedent || consequent
    (.<=>.) = (==)

-- ChatGPT
instance BooleanLogic SBV.SBool where
    true = SBV.sTrue
    false = SBV.sFalse
    b_not = SBV.sNot
    (.&&.) = (SBV..&&)
    (.||.) = (SBV..||)
    (.<+>.) = (SBV..<+>)
    (.=>.) = (SBV..=>)
    (.<=>.) = (SBV..<=>)

-- | Conditional selection shared by concrete and symbolic conditions.
-- Z3-tested through the symbolic sorting and polygon containment properties.
class LogicIte condition value where
    logicIte :: condition -> value -> value -> value

-- ChatGPT
instance LogicIte Bool value where
    logicIte condition whenTrue whenFalse =
        if condition then whenTrue else whenFalse

instance SBV.Mergeable value => LogicIte SBV.SBool value where
    logicIte = SBV.ite

-- ChatGPT
instance {-# OVERLAPPABLE #-} (Eq a, LogicBoolean a ~ Bool) => LogicEq a where
    (.==.) = (==)
    (./=.) = (/=)

-- ChatGPT
instance {-# OVERLAPPING #-} LogicEq SBV.SReal where
    (.==.) = (SBV..==)
    (./=.) = (SBV../=)

-- ChatGPT
instance {-# OVERLAPPABLE #-} (Ord a, LogicBoolean a ~ Bool) => LogicOrd a where
    (.<.) = (<)
    (.<=.) = (<=)
    (.>.) = (>)
    (.>=.) = (>=)

-- ChatGPT
instance {-# OVERLAPPING #-} LogicOrd SBV.SReal where
    (.<.) = (SBV..<)
    (.<=.) = (SBV..<=)
    (.>.) = (SBV..>)
    (.>=.) = (SBV..>=)

-- ChatGPT
-- Z3-tested indirectly by the polygon containment properties.
logicAll :: BooleanLogic b => [b] -> b
logicAll = foldr (.&&.) true

-- ChatGPT
-- Z3-tested by symbolicLogicSortOrdersThree and the polygon properties.
logicAny :: BooleanLogic b => [b] -> b
logicAny = foldr (.||.) false

-- ChatGPT
-- | Insertion sort whose comparisons and conditional swaps may be symbolic.
-- The list length is fixed; symbolic conditions select which value occupies
-- each output position.
-- Z3-tested by symbolicLogicSortOrdersThree and the polygon properties.
logicSort
    :: (LogicOrd a, LogicIte (LogicBoolean a) a)
    => [a]
    -> [a]
logicSort = logicSortOn id

-- ChatGPT
-- Z3-tested by symbolicLogicSortOrdersThree and the polygon properties.
logicSortOn
    :: (LogicOrd key, LogicIte (LogicBoolean key) value)
    => (value -> key)
    -> [value]
    -> [value]
logicSortOn f = foldr insert []
  where
    insert value [] = [value]
    insert value (next:rest) =
        smaller : insert larger rest
      where
        valueComesFirst = f value .<=. f next
        smaller = logicIte valueComesFirst value next
        larger = logicIte valueComesFirst next value

-- Z3-tested by the polygon containment properties.
logicSortOnFiltered
    :: (boolean ~ LogicBoolean key, LogicOrd key, LogicIte boolean (boolean, value))
    => (value -> key)
    -> [(boolean, value)]
    -> [(boolean, value)]
logicSortOnFiltered f = foldr insert []
  where
    insert value [] = [value]
    insert value'@(b1,value) (next'@(b2,next):rest) =
        smaller : insert larger rest
      where
        valueComesFirst = b1 .&&. (b_not b2 .||. f value .<=. f next)
        smaller = logicIte valueComesFirst value' next'
        larger = logicIte valueComesFirst next' value'

-- ChatGPT
-- | True exactly when an odd number of elements are true.
--
-- XOR is true exactly when its two inputs differ:
--
-- @
-- False XOR False = False
-- False XOR True  = True
-- True  XOR False = True
-- True  XOR True  = False
-- @
--
-- Treat the accumulator as answering:
--
-- /Have I seen an odd number of crossings?/
--
-- Start with 'False', because zero crossings is even.
-- For every edge:
--
-- * No crossing ('False') -> leave the answer unchanged.
-- * Crossing ('True') -> flip the answer.
--
-- >>> oddParity [True, False, False]
-- True
-- >>> oddParity [True, False, True]
-- False
--
-- The second example reduces from right to left as follows:
--
-- @
-- foldr (.<+>.) False [True, False, True]
--   = True  .<+>. (False .<+>. (True .<+>. False))
--   = True  .<+>. (False .<+>. True)
--   = True  .<+>. True
--   = False
-- @
-- Z3-tested by the pointInPolygon properties.
oddParity :: BooleanLogic b => [b] -> b
oddParity = foldr (.<+>.) false

-- by ChatGPT
newtype OnePerLine a = OnePerLine [a]

-- by ChatGPT
instance Show a => Show (OnePerLine a) where
  show :: Show a => OnePerLine a -> String
  show (OnePerLine xs) = unlines (map show xs)

type Parser = Parsec Void Text

-- This uses just for smart type aliases
type family Point a
type instance Point a = (a,a)

type family Edge a
type instance Edge a = (Point a, Point a)

type PolygonVertices = [Point Int]
type ParsedType = PolygonVertices

pNumber :: forall a. Read a => Parser a
pNumber = read <$> some digitChar

pNumberLine :: Parser (Point Int)
pNumberLine = do
    x <- pNumber
    char ','
    y <- pNumber
    eol
    return (x,y)

parser :: Parser [Point Int]
parser = some pNumberLine

area :: Num a => Point a -> Point a -> a
area (x1,y1) (x2,y2) = (abs dx + 1) * (abs dy + 1) where
  dx = x1 - x2 
  dy = y1 - y2

part1 :: [Point Int] -> Int
part1 xs =  case ys of
        (y:_) -> y
        _ -> 0
    where
        ys = sortOn Down $ fmap (uncurry area) ((\[a,b] -> (a,b)) <$> choose 2 xs)

-- Z3-tested by the polygon containment properties.
pointScalarMult :: Num a => a -> Point a  -> Point a
pointScalarMult a (x,y) = (a*x,a*y)

-- Z3-tested by the half-ray and polygon properties.
pointSum :: Num a => Point a -> Point a -> Point a
pointSum (x1,y1) (x2,y2) = (x1+x2,y1+y2)

-- Z3-tested by solve2x2JustResultSolvesSystem, nonzeroDeterminantImpliesOneSolution,
-- segmentIntersectionMatchesParametricSolution, and
-- parametricIntersectionImpliesNonzeroDeterminant.
det2x2 :: Num a => (Point a, Point a) -> a
det2x2 ((a1,b1), (a2,b2)) = a1*b2 - a2*b1

-- |
-- Cramers rule
-- solve
-- a1 b1 x x = c1
-- a2 b2   y   c2 
-- Z3-tested by solve2x2JustResultSolvesSystem and
-- segmentIntersectionMatchesParametricSolution.
solve2x2' :: Fractional a => (Point a, Point a) -> Point a -> a -> Point a
solve2x2' a@((a1,b1),(a2,b2)) (c1,c2) det = ( (c1*b2-b1*c2) / det, (a1*c2 - c1 * a2) / det )

-- |
-- Cramers rule
-- Returns whether the system has a unique solution and the solution calculated
-- by Cramer's rule. The returned point is meaningful only when the Boolean is
-- true. Keeping the condition as a value allows the same function to work with
-- both Bool and SBV.SBool.
-- Z3-tested by solve2x2JustResultSolvesSystem and the geometry properties.
solve2x2 :: (Fractional a, LogicEq a) => (Point a, Point a) -> Point a -> (LogicBoolean a, Point a)
solve2x2 a c = (det ./=. 0, res) where
    det = det2x2 a
    res = solve2x2' a c det

{--
(math help from chatgpt)

Obtain a general equation of a line on the plane

vector from p1 to p2
P1 -> P2      = (x2 - x1, y2 - y1)
vector from p1 to arbitrary point (w,z)
P1 -> (w,z)   = (w - x1, z - y1)

for (w,z) to be on the line the vectors must be parallel,
can be checked by determinant of vectors as lines of matrix is zero

| x2 - x1    y2 - y1 |
| w  - x1    z  - y1 | = 0

(y2 - y1)*w + (x1 - x2)*z = x1*y2 - x2*y1
--}
-- Z3-tested by the segment-intersection and half-ray properties.
lineIntersectionMatrix :: Num a => Edge a -> Edge a -> (Point a, Point a)
lineIntersectionMatrix ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
    ((y2 - y1, x1-x2), (y4 - y3, x3 - x4))

-- Z3-tested by the segment-intersection and half-ray properties.
lineIntersectionConstants :: Num a => Edge a -> Edge a -> Point a
lineIntersectionConstants ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
    (x1 * y2 - x2 * y1, x3 * y4 - x4 * y3)

-- Z3-tested indirectly by the geometry properties through isInInterval.
booleanIte :: (BooleanLogic b) => b -> b -> b -> b
booleanIte cond whenTrue whenFalse =
    (cond .&&. whenTrue) .||. (b_not cond .&&. whenFalse)

-- Z3-tested indirectly by the geometry properties through isInRect.
isInInterval :: LogicOrd a => a -> a -> a -> LogicBoolean a
isInInterval x1 x2 x3 =
        booleanIte
        (x1 .<=. x2) 
        (x1 .<=. x3 .&&. x3 .<=. x2)
        (x2 .<=. x3 .&&. x3 .<=. x1)

-- Z3-tested by the point, segment-intersection, half-ray, and polygon properties.
isInRect :: LogicOrd a => Edge a -> Point a -> LogicBoolean a
isInRect ((x1,y1), (x2,y2)) (x3,y3) =
    isInInterval x1 x2 x3 .&&. isInInterval y1 y2 y3

-- Z3-tested by the point, segment-intersection, half-ray, and polygon properties.
pointEq :: LogicEq a => Point a -> Point a -> LogicBoolean a
pointEq (x1, y1) (x2, y2) = x1 .==. x2 .&&. y1 .==. y2


-- Z3-tested by symbolicProperSegmentIntersectionExamples.
hasLineSegmentsIntersectAtInteriorPoint
    :: (Fractional a, LogicOrd a)
    => Edge a
    -> Edge a
    -> LogicBoolean a
hasLineSegmentsIntersectAtInteriorPoint edgeA edgeB = fst $ lineSegmentIntersectionAtInteriorPoint edgeA edgeB

-- Z3-tested indirectly by symbolicProperSegmentIntersectionExamples.
lineSegmentIntersectionAtInteriorPoint
    :: (Fractional a, LogicOrd a)
    => Edge a
    -> Edge a
    -> (LogicBoolean a, Point a)
lineSegmentIntersectionAtInteriorPoint edgeA@(p1, p2) edgeB@(p3, p4) = (intersects, intersection)
  where
    (hasUniqueIntersection, intersection) =
        solve2x2
            (lineIntersectionMatrix edgeA edgeB)
            (lineIntersectionConstants edgeA edgeB)
    intersects = b_not (pointEq p1 p2)
        .&&. b_not (pointEq p3 p4)
        .&&. hasUniqueIntersection
        .&&. b_not (pointEq intersection p1)
        .&&. b_not (pointEq intersection p2)
        .&&. b_not (pointEq intersection p3)
        .&&. b_not (pointEq intersection p4)
        .&&. isInRect edgeA intersection
        .&&. isInRect edgeB intersection

    

-- | Return this edge's contribution to the odd-even crossing count for the
-- horizontal half-ray starting at the point and going right.
--
-- This is not ordinary set-theoretic segment/half-ray intersection.  A strict
-- interior intersection is counted when it lies to the right of the tested
-- point.  An intersection at a segment endpoint uses a half-open convention:
-- it is counted only when the segment's other endpoint is at or below the ray
-- (that is, at or below @y3@).
--
-- The endpoint convention is meaningful when the results for all polygon
-- edges are combined by 'oddParity'.  At a polygon vertex, the two incident
-- edges then contribute:
--
-- * zero crossings when both other endpoints are above the ray;
-- * one crossing when one is above and one is below;
-- * two crossings when both are below.
--
-- Thus a genuine crossing changes parity once, while a vertex at which the
-- polygon merely touches the ray changes it zero or two times.  Whether the
-- tested point itself lies on the polygon boundary is a separate question.
-- Z3-tested by the half-ray endpoint properties and the pointInPolygon properties.
lineSegmentIntersectsHalfRayGoingRight
    :: (Fractional a, LogicOrd a)
    => Point a
    -> Edge a
    -> LogicBoolean a
lineSegmentIntersectsHalfRayGoingRight pointInAnalysis@(x3, y3) edge@(p1@(_,y1), p2@(_,y2)) =
    b_not (pointEq p1 p2)
        .&&. hasUniqueIntersection
        .&&. x3 .<. intersectionX
        .&&. (
            (b_not (pointEq intersection p1)
                .&&. b_not (pointEq intersection p2)
                .&&. b_not (pointEq intersection pointInAnalysis)
                .&&. isInRect edge intersection
                ) .||.
            ((pointEq intersection p1) .&&. (y2 .<=. y3)) .||. 
            ((pointEq intersection p2) .&&. (y1 .<=. y3)))
  where
    ray = (pointInAnalysis, pointSum pointInAnalysis (1, 0))
    (hasUniqueIntersection, intersection@(intersectionX, _)) =
        solve2x2
            (lineIntersectionMatrix edge ray)
            (lineIntersectionConstants edge ray)

memoIntegralPoint :: Integral a => Memo (Point a)
memoIntegralPoint = Memo.pair Memo.integral Memo.integral

memoRational :: Memo Rational
memoRational = Memo.wrap to from memoIntegralPoint where
    to (a,b) = a % b
    from r = (numerator r , denominator r)

memoRationalPointPoint :: Memo (Point Rational)
memoRationalPointPoint = Memo.pair memoRational memoRational

memoEdge :: Memo (Edge Rational)
memoEdge = Memo.pair memoRationalPointPoint memoRationalPointPoint

memoLineSegmentIntersectsHalfRayGoingRight :: Point Rational -> Edge Rational -> Bool
memoLineSegmentIntersectsHalfRayGoingRight =
    Memo.memo2 memoRationalPointPoint memoEdge lineSegmentIntersectsHalfRayGoingRight

verticesToEdges' :: [Point a] -> [Edge a]
verticesToEdges' xs = zipWith f xs (drop 1 $ cycle xs) where
    n = length xs
    f a b = (a,b)

toRational' :: Integral a => a -> Rational
toRational' = fromIntegral

intToRational :: Int -> Rational
intToRational = fromIntegral

intToDouble :: Int -> Double
intToDouble = fromIntegral

{--
MN thinking:
parametric version of line segment
s(t) = p1 + (p2-p1)*t

p = s(t)
<=> p = p1 + (p2-p1)*t
<=> 
    (x - x1) = (x2-x1)t ∧
    (y - y1) = (y2-y1)t
help from chagpt: cross-multiply as to not divide by ZERO
<=> 
    (x - x1)(y2-y1) = (x2-x1)(y2-y1)t ∧
    (y - y1)(x2-x1) = (y2-y1)(x2-x1)t

=> 
(a = b && b = c => a = c transitivity)
p = s(t) => (x - x1)(y2-y1) = (y - y1)(x2-x1)

This is the main idea. But actually the other direction works too.
Proof by chagpt:

Let:

p₁ = (x₁, y₁)
p₂ = (x₂, y₂)
p  = (x, y)

Definition of the closed line segment:

p ∈ [p₁, p₂]
⇔
∃t ∈ [0,1] such that p = p₁ + t(p₂ − p₁)

Define:

C ⇔ (x − x₁)(y₂ − y₁) = (y − y₁)(x₂ − x₁)

B ⇔ min(x₁,x₂) ≤ x ≤ max(x₁,x₂)
  ∧ min(y₁,y₂) ≤ y ≤ max(y₁,y₂)

We prove:

p ∈ [p₁,p₂] ⇔ C ∧ B


Forward direction: p ∈ [p₁,p₂] ⇒ C ∧ B

Assume p ∈ [p₁,p₂].

Then:

∃t ∈ [0,1] such that p = p₁ + t(p₂ − p₁)

Therefore:

x − x₁ = t(x₂ − x₁)
y − y₁ = t(y₂ − y₁)

Multiplying the first equation by y₂ − y₁:

(x − x₁)(y₂ − y₁)
  = t(x₂ − x₁)(y₂ − y₁)

Multiplying the second equation by x₂ − x₁:

(y − y₁)(x₂ − x₁)
  = t(y₂ − y₁)(x₂ − x₁)

The right-hand sides are equal, so:

(x − x₁)(y₂ − y₁)
  = (y − y₁)(x₂ − x₁)

Therefore C holds.

For the x-coordinate:

x = (1 − t)x₁ + tx₂

If x₁ ≤ x₂:

x − x₁ = t(x₂ − x₁) ≥ 0
x₂ − x = (1 − t)(x₂ − x₁) ≥ 0

Therefore:

x₁ ≤ x ≤ x₂

If x₂ ≤ x₁, the same argument with the endpoints reversed gives:

x₂ ≤ x ≤ x₁

Hence:

min(x₁,x₂) ≤ x ≤ max(x₁,x₂)

The same argument gives:

min(y₁,y₂) ≤ y ≤ max(y₁,y₂)

Therefore B holds.

Thus:

p ∈ [p₁,p₂] ⇒ C ∧ B


Reverse direction: C ∧ B ⇒ p ∈ [p₁,p₂]

Assume C ∧ B.

Case 1: p₁ = p₂

Then B describes a bounding rectangle containing only p₁.

Therefore:

p = p₁

Choose t = 0. Then:

p = p₁ + 0(p₂ − p₁)

Thus:

p ∈ [p₁,p₂]


Case 2: p₁ ≠ p₂

At least one of these is nonzero:

x₂ − x₁ ≠ 0
or
y₂ − y₁ ≠ 0

If x₂ − x₁ ≠ 0, define:

t = (x − x₁)/(x₂ − x₁)

Condition C implies:

y − y₁ = t(y₂ − y₁)

Therefore:

p = p₁ + t(p₂ − p₁)

Since x lies between x₁ and x₂, we have:

0 ≤ t ≤ 1

If x₂ − x₁ = 0, then y₂ − y₁ ≠ 0. Define:

t = (y − y₁)/(y₂ − y₁)

Condition C implies:

x = x₁ = x₂

Since y lies between y₁ and y₂:

0 ≤ t ≤ 1

Therefore, in either case:

∃t ∈ [0,1] such that p = p₁ + t(p₂ − p₁)

Thus:

C ∧ B ⇒ p ∈ [p₁,p₂]

Conclusion:

p ∈ [p₁,p₂]
⇔
(x − x₁)(y₂ − y₁) = (y − y₁)(x₂ − x₁)
∧
min(x₁,x₂) ≤ x ≤ max(x₁,x₂)
∧
min(y₁,y₂) ≤ y ≤ max(y₁,y₂)

Also proved with SBV
--}
-- Z3-tested by the pointOnEdge and polygon properties.
pointOnEdge :: (Num a, LogicOrd a) => Edge a -> Point a -> LogicBoolean a
pointOnEdge edge@(p1@(x1,y1),p2@(x2,y2)) p@(x,y) = 
    pointEq p p1
        .||. pointEq p p2
        .||. (
            (pointOnLine edge p) .&&.
            (isInRect edge p)
        )


-- Z3-tested by the pointOnLine, pointOnEdge, and polygon properties.
pointOnLine :: (Num a, LogicOrd a) => Edge a -> Point a -> LogicBoolean a
pointOnLine (p1@(x1,y1),p2@(x2,y2)) p@(x,y) = (x - x1)*(y2-y1) .==. (y - y1)*(x2-x1)

-- Z3-tested indirectly by the pointInPolygon properties.
pointOnAnyEdge :: (Num a, LogicOrd a) => [Edge a] -> Point a -> LogicBoolean a
pointOnAnyEdge edges p = logicAny $ fmap ((flip pointOnEdge) p) edges

-- |
-- Rational should not have precision problems, but maybe too slow.
polygonEdgesProperlyIntersect
    :: (Fractional a, LogicOrd a)
    => [Edge a]
    -> [Edge a]
    -> LogicBoolean a
polygonEdgesProperlyIntersect edgesA edgesB =
    logicAny
        [ hasLineSegmentsIntersectAtInteriorPoint edgeA edgeB
        | edgeA <- edgesA
        , edgeB <- edgesB
        ]

-- |
-- 1. Shoot half ray from point in any direction, for instance along the X axis.
-- 2. Count how many times it crosses an edge of the polygon P 
-- 3. if the result is even then it is outside, if it is odd it is inside.
-- ChatGPT figure:
-- Inside example:
--                         polygon P
--                      +------------+
--                     /              \
--                    /                \
--                   /                  \
--                  +                    +
--                  |                    |
--                  |        p ----------+-----------> half-ray along +X
--                  |             x1     |
--                  |                    |
--                  +--------------------+
--
--
--      outside example:
--
--                         polygon P
--                      +-------------+
--                     /               \
--                    /                 \
--                   /                   \
--          p ------+---------------------+---------->
--                  |                     |
--                  |                     |    
--                  |                     |             
--                  |                     |
--                  +---------------------+
--                      1.036 s ±  70 ms

-- Z3-tested by symbolicPointInPolygonMatchesRectangleCheck and
-- symbolicPointInPolygonTriangleCounterexample.
pointInPolygon
    :: (Fractional a, LogicOrd a)
    => [Edge a]
    -> Point a
    -> LogicBoolean a
pointInPolygon edges point = pointOnAnyEdge edges point .||. isInside where
    isInside = oddParity (map (lineSegmentIntersectsHalfRayGoingRight point) edges)

memoPointInPolygon
    :: (Fractional a, Integral b, LogicOrd a, LogicBoolean a ~ Bool)
    => [Edge a]
    -> Point b
    -> Bool
memoPointInPolygon edges p = memoIntegralPoint memoPointInPolygon' p where
    memoPointInPolygon' = pointInPolygon edges . over both fromIntegral

-- |
-- This assumes the line segments are not degenerate => p1 /= p2 && p3 /= p4.
-- It also returns empty list if the intersection is any of the endpoints of ls.
-- Z3-tested by lineSegmentEdgeIntersectionPointsSoundness and the polygon
-- containment properties.
lineSegmentEdgeIntersectionPoints
    :: forall a b .
    (Fractional a
    , LogicOrd a
    , b ~ LogicBoolean a
    , LogicIte b ([Point a])
    , LogicIte b ([(b, Point a)])
    , LogicIte b (Point a))
    => Edge a
    -> Edge a
    -> [(b, Point a)]
lineSegmentEdgeIntersectionPoints ls@(p1, p2) edge@(p3, p4) =  
    logicIte segmentsIntersectInOnePoint  
    (logicIte isAnEndPointOfLs dummyPointList [(true, intersection), dummyPoint])
    (logicIte segmentsColinear (logicIte segmentsColinarButOnlyIntersectInOnePoint dummyPointList (logicIte intersectsColinearAndOverlaps [(b_not (pointEq p5 p1 .||. pointEq p5 p2), p5), (b_not (pointEq p6 p1 .||. pointEq p6 p2), p6)] dummyPointList)) dummyPointList)
  where
    dummyPoint = (false, (0,0))
    dummyPointList = [dummyPoint, dummyPoint]
    isAnEndPointOfLs = (pointEq intersection p1) .||. (pointEq intersection p2)
    (linesIntersectInOnePoint, intersection) =
        solve2x2
            (lineIntersectionMatrix ls edge)
            (lineIntersectionConstants ls edge)
    segmentsIntersectInOnePoint = 
            linesIntersectInOnePoint
        .&&. isInRect ls intersection
        .&&. isInRect edge intersection 
    segmentsColinear = b_not linesIntersectInOnePoint .&&. pointOnLine ls p3
    segmentsColinarButOnlyIntersectInOnePoint = pointEq p1 p3 .||. pointEq p1 p4 .||. pointEq p2 p3 .||. pointEq p2 p4 
    intersectsColinearAndOverlaps = (isInRect ls p3 .||. isInRect ls p4 .||. isInRect edge p1 .||. isInRect edge p2)
    [_,p5,p6,_] = sortPointsOnLineSegment ls [p1,p2,p3,p4]

-- |
-- Either the line segment is vertical and then we sort by the Y coordinate, or it is not vertical and X values will have different values,
-- so we sort using the X coordinate.
-- Z3-tested indirectly by lineSegmentEdgeIntersectionPointsSoundness.
sortPointsOnLineSegment 
    :: ( LogicOrd a
       , b ~ LogicBoolean a
       , LogicIte b ([Point a])
       , LogicIte b (Point a)
       ) =>
    Edge a -> [Point a] -> [Point a]
sortPointsOnLineSegment ((x1,_),(x2,_)) xs =
    logicIte (x1 .==. x2) sortY sortX where
        sortX = logicSortOn fst xs
        sortY = logicSortOn snd xs

-- |
-- Either the line segment is vertical and then we sort by the Y coordinate, or it is not vertical and X values will have different values,
-- so we sort using the X coordinate.
-- Z3-tested by the polygon containment properties.
sortPointsOnLineSegmentFiltered 
    :: ( LogicOrd a
       , b ~ LogicBoolean a
       , LogicIte b ([Point a])
       , LogicIte b (Point a)
       , LogicIte b [(b, (a, a))]
       , LogicIte b (b, (a, a))
       ) =>
    Edge a -> [(b, Point a)] -> [(b, Point a)]
sortPointsOnLineSegmentFiltered ((x1,_),(x2,_)) xs =
    logicIte (x1 .==. x2) sortY sortX where
        sortX = logicSortOnFiltered fst xs
        sortY = logicSortOnFiltered snd xs

-- |
-- Algorithm:
--
-- For a an edge e:

-- 1. Find all intersections between e and the polygon boundary.
-- 2. Add the two endpoints of e.
-- 3. For collinear overlaps, add the overlap endpoints.
-- 4. Sort all these points along e.
-- 5. Test:
--    - every cut point;
--    - the midpoint between every consecutive pair.
-- Require every tested point to be inside or on boundary.
--
-- Z3-tested by the polygon containment properties.
lineSegmentIsInsideOrOn
    :: forall a b . (Fractional a, LogicOrd a
    , b ~ LogicBoolean a
    , LogicIte b (Point a)
    , LogicIte b ([Point a])
    , LogicIte b ([(LogicBoolean a, Point a)])
    , LogicIte b (b, (a, a)))
    =>
    [Edge a]
    -> Edge a
    -> b
lineSegmentIsInsideOrOn edges segment@(a,b) = logicAll $ fmap g all where
    intersections :: [(b,Point a)]
    intersections = concatMap (lineSegmentEdgeIntersectionPoints segment) edges
    intersectionsAndEndpoints :: [(b,Point a)]
    intersectionsAndEndpoints = intersections ++ [(true,a),(true,b)]
    sorted = sortPointsOnLineSegmentFiltered segment intersectionsAndEndpoints
    midpoints = zipWith f sorted (drop 1 sorted) -- last point of sorted will not be used on the second argument of zipWith
    -- this works because the not present points are all at then end of the sorted list
    f (b1,p1) (b2,p2) = (b1 .&&. b2, pointScalarMult 0.5 (pointSum p1 p2))
    all = intersectionsAndEndpoints ++ midpoints
    g (boolean, value) = boolean .=>. pointInPolygon edges value

-- |
-- If i keep this version I can't use memoization.
-- Z3-tested by polygonIsInsideOrOnTriangleSoundnessCounterexample and
-- polygonIsInsideOrOnTriangleCompletenessCounterexample.
polygonIsInsideOrOn
    ::(
        LogicBoolean a ~ b
        , Fractional a
        , LogicOrd a
        , LogicIte b (a, a)
        , LogicIte b (b, (a, a))
        , LogicIte b [(a, a)]
        , LogicIte b [(b, (a, a))])
    => [Edge a]
    -> [Edge a]
    -> b
polygonIsInsideOrOn edgesA edgesB = logicAll $ fmap (lineSegmentIsInsideOrOn edgesB) edgesA

--  a +-------------+ d
--    |             |
--  c +-------------+ b
makeRectangle :: Point Int -> Point Int -> ([Point Int], [Edge Double])
makeRectangle a@(x1,y1) b@(x2,y2) = (vertices, edges) where
    c = (x1,y2)   
    d = (x2,y1)
    vertices = [a,d,b,c]
    edges = verticesToEdges vertices

-- |
-- Rational would give guarantees of no rounding error, but is much more expensive.
-- Double gives the right solution in my input    
verticesToEdges :: [Point Int] -> [Edge Double]
verticesToEdges = verticesToEdges' . over (traversed.each) intToDouble

part2 :: PolygonVertices -> Int
part2 vertices = case areas of
        (x:_) -> x
        [] -> 0
    where
        -- calculate edges only once. this includes converting to fractional type
        edges = verticesToEdges vertices
        polygon = (vertices, edges)
        rectanglesInsidePolygon = do
            v@(x1,y1) <- vertices
            w@(x2,y2) <- vertices
            let (_,rectangle) = makeRectangle v w
            guard $ x1 /= x2 && y1 /= y2 && polygonIsInsideOrOn rectangle edges
            return (v,w)
        areas = sortOn Down $ fmap (uncurry area) rectanglesInsidePolygon

{-- TODO
- lineSegmentIsInsideOrOn continue

- Replace vertex containment + no proper crossings with complete-edge containment.
- Split each tested edge at boundary intersections/overlaps and check each interval.
- Make the concave-notch regression pass, then restore the containment SBV tests.
- Benchmark and reintroduce caching where it materially helps.
--}
