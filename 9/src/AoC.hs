{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
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
used chatgpt: yes, to research computacional geometry alrightms in general, but also wikipedia. It was used to determine the algorithmn for the function polygonInsidePolygonTouchingOk. ChatGPT as used to help create sbv proofs of correctness for my hand-coded functions.  All the code in thihs file is my own.
notes:
I decided to first research a bit the algorithms used for this, as this seemed a bit too far away from the usual CS algorithms.
The algorithm was a too tricky to find on my own because touching edges and vertices must be allowed. In the end I needed help for the algorithmn of one of the functions. The first try was correct, clocking at 4m execution time. Will try to make it more eficient.

times:
1st attempt: 4m
2nd attempt 2.4s calculated edges only once and changed from Rationals to Floats
3rd attempt 1,0s memoization of pointInPolygon

  part1 without parsing: OK
    33.5 ms ± 397 μs
  part2 without parsing: OK
    2.302 s ±  61 ms
  part1 with parsing:    OK
    37.1 ms ± 2.4 ms
  part2 with parsing:    OK
    2.321 s ±  72 ms

todo: improve speed
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
import GHC.Base (leInt)
import Data.Ratio (numerator, denominator, (%))
import qualified Data.MemoCombinators.Class as Memo

-- by ChatGPT
newtype OnePerLine a = OnePerLine [a]

-- by ChatGPT
instance Show a => Show (OnePerLine a) where
  show :: Show a => OnePerLine a -> String
  show (OnePerLine xs) = unlines (map show xs)

type Parser = Parsec Void Text

type Point = (Int,Int)
type Vertice = Point
type PolygonVertices = [Vertice]
type Edge = ((Float, Float), (Float, Float))
type ParsedType = PolygonVertices

pNumber :: forall a. Read a => Parser a
pNumber = read <$> some digitChar

pNumberLine :: Parser Point
pNumberLine = do
    x <- pNumber
    char ','
    y <- pNumber
    eol
    return (x,y)

parser :: Parser ParsedType
parser = some pNumberLine

area :: Num a => (a, a) -> (a, a) -> a
area (x1,y1) (x2,y2) = (abs dx + 1) * (abs dy + 1) where
  dx = x1 - x2 
  dy = y1 - y2

part1 :: ParsedType -> Int
part1 xs =  case ys of
        (y:_) -> y
        _ -> 0
    where
        ys = sortOn Down $ fmap (uncurry area) ((\[a,b] -> (a,b)) <$> choose 2 xs)

pointSum :: Num a => (a,a) -> (a,a) -> (a,a)
pointSum (x1,y1) (x2,y2) = (x1+x2,y1+y2)

det2x2 :: Num a => ((a,a),(a,a)) -> a
det2x2 ((a1,b1), (a2,b2)) = a1*b2 - a2*b1

-- |
-- Cramers rule
-- solve
-- a1 b1 x x = c1
-- a2 b2   y   c2 
solve2x2' :: Fractional a => ((a,a),(a,a)) -> (a,a) -> a -> (a,a)
solve2x2' a@((a1,b1),(a2,b2)) (c1,c2) det = ( (c1*b2-b1*c2) / det, (a1*c2 - c1 * a2) / det )

-- |
-- Cramers rule
-- Returns a unique solution if it exists. If there are many solutions or no solution returns Nothing.
solve2x2 :: (Fractional a, Eq a) => ((a,a),(a,a)) -> (a,a) -> Maybe (a,a)
solve2x2 a@((a1,b1),(a2,b2)) c@(c1,c2) = if det /= 0 then Just res else Nothing where
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
lineIntersectionMatrix :: Num a => ((a, a), (a, a)) -> ((a, a), (a, a)) -> ((a, a), (a, a))
lineIntersectionMatrix ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
    ((y2 - y1, x1-x2), (y4 - y3, x3 - x4))

lineIntersectionConstants :: Num a => ((a, a), (a, a)) -> ((a, a), (a, a)) -> (a, a)
lineIntersectionConstants ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
    (x1 * y2 - x2 * y1, x3 * y4 - x4 * y3)

isInInterval :: Ord a => a -> a -> a -> Bool
isInInterval x1 x2 x3 = if x1 <= x2 then (x1 <= x3 && x3 <= x2) else (x2 <= x3 && x3 <= x1)

isInRect :: (Ord a1, Ord a2) => ((a1, a2), (a1, a2)) -> (a1, a2) -> Bool
isInRect ((x1,y1), (x2,y2)) (x3,y3) = isInInterval x1 x2 x3 && isInInterval y1 y2 y3

lineSegmentsIntersectAtInteriorPoint :: (Ord a2, Fractional a2, Show a2) => ((a2, a2), (a2, a2)) -> ((a2, a2), (a2, a2)) -> Bool
lineSegmentsIntersectAtInteriorPoint a@(p1, p2) b@(p3, p4)
    | p1 == p2 = error $ "lineSegmentsIntersectAtInteriorPoint: first segment has identical endpoints: " <> show p1 <> " " <> show p2
    | p3 == p4 = error "lineSegmentsIntersectAtInteriorPoint: second segment has identical endpoints"
    | otherwise = case sol of
        -- Unique solution
        Just p
            | p == p1 || p == p2 || p == p3 || p == p4 -> False
            | not (isInRect a p && isInRect b p) -> False
            | otherwise -> True
        -- No solution or multiple solutions
        Nothing -> False
    where
        sol = solve2x2 (lineIntersectionMatrix a b) (lineIntersectionConstants a b)

lineSegmentIntersectsHalfRayGoingRight :: (Ord a, Fractional a) => (a,a) -> ((a, a), (a, a)) -> Bool
lineSegmentIntersectsHalfRayGoingRight p3@(x3,_) a@(p1, p2)
    | p1 == p2 = error "lineSegmentsIntersectAtInteriorPoint: first segment has identical endpoints"
    | otherwise = case sol of
        -- Unique solution
        Just (p@(x,y))
            | p == p1 || p == p2 || p == p3 -> False
            | not (isInRect a p && x3 < x ) -> False
            | otherwise -> True
        -- No solution or multiple solutions
        Nothing -> False
    where
        b = (p3, pointSum p3 (1,0)) 
        sol = solve2x2 (lineIntersectionMatrix a b) (lineIntersectionConstants a b)

memoIntegralPoint :: Integral a => Memo (a,a)
memoIntegralPoint = Memo.pair Memo.integral Memo.integral

memoRational :: Memo Rational
memoRational = Memo.wrap to from memoIntegralPoint where
    to (a,b) = a % b
    from r = (numerator r , denominator r)

memoRationalPointPoint :: Memo (Rational,Rational)
memoRationalPointPoint = Memo.pair memoRational memoRational

memoEdge :: Memo ((Rational,Rational), (Rational,Rational))
memoEdge = Memo.pair memoRationalPointPoint memoRationalPointPoint

memoLineSegmentIntersectsHalfRayGoingRight = Memo.memo2 memoRationalPointPoint memoEdge lineSegmentIntersectsHalfRayGoingRight

verticesToEdges' :: [a] -> [(a, a)]
verticesToEdges' xs = zipWith f xs (drop 1 $ cycle xs) where
    n = length xs
    f a b = (a,b)

toRational' :: Integral a => a -> Rational
toRational' = fromIntegral

intToRational :: Int -> Rational
intToRational = fromIntegral

intToFloat :: Int -> Float
intToFloat = fromIntegral

-- |
-- Rational should not have precision problems, but maybe too slow.
polygonEdgesProperlyIntersect edges_a edges_b = not $ null intesectingEdges where
    intesectingEdges = do
        edge_a <- edges_a
        edge_b <- edges_b
        guard $ lineSegmentsIntersectAtInteriorPoint edge_a edge_b
        return (edge_a, edge_b)

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
pointInPolygon edges p = odd numberOfCrossings where
    p' = over both intToFloat p
    numberOfCrossings = length $ filter f edges
    f edge = lineSegmentIntersectsHalfRayGoingRight p' edge


memoPointInPolygon edges p = memoIntegralPoint g p where
    g p = odd numberOfCrossings where
        p' = over both intToFloat p
        numberOfCrossings = length $ filter f edges
        f edge = lineSegmentIntersectsHalfRayGoingRight p' edge

-- |
-- After tring a couple of times on my own, I didn't manage to find a criteria that worked, so I asked ChatGPT for help:
-- 1. Every vertex of A must be inside-or-on BintToRational.
-- 2. No edge of A may strictly/properly cross an edge of B.
-- 3. Ignore boundary touches and collinear overlaps.
-- Todo: check in SBV
polygonInsidePolygonTouchingOk a@(verticesA@(a1:a2:a3:_),edges_a) b@(verticesB@(b1:b2:b3:_),edges_b) = everyVerticeOfAinsideB && not (polygonEdgesProperlyIntersect edges_a edges_b) where
    everyVerticeOfAinsideB = all (pointInPolygon edges_b) (verticesA) -- OPTIMIZATION the first two vertices are for sure inside the polygon because they are vertices of B
polygonInsidePolygonTouchingOk _ _ = error "polygonInsidePolygonTouchingOk: Both polygons must have 3 vertices"

--  a +-------------+ c
--    |             |
--  d +-------------+ b
makeRectangle a@(x1,y1) b@(x2,y2) = (vertices, edges) where
    c = (x1,y2)   
    d = (x2,y1)
    vertices = [a,b,c,d]
    edges = verticesToEdges vertices
    
verticesToEdges = verticesToEdges' . over (traversed.each) intToFloat

part2 :: PolygonVertices -> Int
part2 vertices = case areas of
        (x:_) -> x
        [] -> 0
    where
        edges = verticesToEdges vertices
        both = (vertices, edges)
        rectanglesInsidePolygon = do
            v@(x1,y1) <- vertices
            w@(x2,y2) <- vertices
            guard $ x1 /= x2 && y1 /= y2 && polygonInsidePolygonTouchingOk (makeRectangle v w) both
            return (v,w)
        areas = sortOn Down $ fmap (uncurry area) rectanglesInsidePolygon































-- Tests

genTestString = do
    s <- readFile "test_input"
    putStrLn $  "tString = " <> show s

tString = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3\n"

tParsed = case parse parser "input" tString of
    Right x -> x
    Left _ -> error "not parsed"

-- >>> OnePerLine tParsed
-- (7,1)
-- (11,1)
-- (11,7)
-- (9,7)
-- (9,5)
-- (2,5)
-- (2,3)
-- (7,3)

-- >>> OnePerLine $ choose 2 tParsed
-- [(7,1),(11,1)]
-- [(7,1),(11,7)]
-- [(7,1),(9,7)]
-- [(7,1),(9,5)]
-- [(7,1),(2,5)]
-- [(7,1),(2,3)]
-- [(7,1),(7,3)]
-- [(11,1),(11,7)]
-- [(11,1),(9,7)]
-- [(11,1),(9,5)]
-- [(11,1),(2,5)]
-- [(11,1),(2,3)]
-- [(11,1),(7,3)]
-- [(11,7),(9,7)]
-- [(11,7),(9,5)]
-- [(11,7),(2,5)]
-- [(11,7),(2,3)]
-- [(11,7),(7,3)]
-- [(9,7),(9,5)]
-- [(9,7),(2,5)]
-- [(9,7),(2,3)]
-- [(9,7),(7,3)]
-- [(9,5),(2,5)]
-- [(9,5),(2,3)]
-- [(9,5),(7,3)]
-- [(2,5),(2,3)]
-- [(2,5),(7,3)]
-- [(2,3),(7,3)]

t1 = sortOn (\(a,b,c) -> Down a) $ fmap (\(a,b) -> (uncurry area (a,b), a, b)) ((\[a,b] -> (a,b)) <$> choose 2 tParsed)

-- >>> OnePerLine t1
-- (15,(11,1),(2,5))
-- (15,(11,7),(2,3))
-- (13,(11,1),(2,3))
-- (13,(11,7),(2,5))
-- (13,(9,7),(2,3))
-- (12,(7,1),(11,7))
-- (11,(7,1),(2,5))
-- (11,(9,7),(2,5))
-- (11,(9,5),(2,3))
-- (10,(7,1),(9,7))
-- (10,(11,1),(9,7))
-- (10,(11,7),(7,3))
-- (9,(7,1),(2,3))
-- (9,(9,5),(2,5))
-- (9,(2,5),(7,3))
-- (8,(7,1),(9,5))
-- (8,(11,1),(11,7))
-- (8,(11,1),(9,5))
-- (8,(11,1),(7,3))
-- (8,(9,7),(7,3))
-- (7,(2,3),(7,3))
-- (6,(7,1),(11,1))
-- (6,(11,7),(9,5))
-- (6,(9,5),(7,3))
-- (4,(7,1),(7,3))
-- (4,(11,7),(9,7))
-- (4,(9,7),(9,5))
-- (4,(2,5),(2,3))
