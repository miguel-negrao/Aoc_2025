{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}

{--
part1
time: 
attempts:
used chatgpt: no
notes: for each tree, generate the list of (presentIndex, pos) with the presents that need to be under it and check until finding a set of positions that fit. This would be very brute-force.

part2
time:
attempts:
used chatgpt: no
notes:

--}

module AoC
    ( Parser
    , parser
    , part1
    , part2
    , ParsedType
    ) where

import Data.List
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad
import Data.Functor
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
import Data.Maybe
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Foldable
import Data.Function (fix)
import Text.Megaparsec.Debug
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Ord (Down(..))
import GHC.Conc (numSparks)

-- vscode on termux on android on portuguese keyboard cannot type [] !! arghhhh!!!
emptyList = []
infiniteList n = [n..]
listFromTo a b = [a..b]

-- by ChatGPT
newtype OnePerLine a = OnePerLine [a]

-- by ChatGPT
instance Show a => Show (OnePerLine a) where
  show :: Show a => OnePerLine a -> String
  show (OnePerLine xs) = unlines (map show xs)

type Parser = Parsec Void Text
type Point = (Int,Int)
type Index = Int
type Shape = Vector Point
-- A flat array where we calculate the 2D -> 1D index conversion
type Area = Vector Bool

convertIndex :: Int -> Point -> Int
convertIndex width (x,y) = y*width + x

-- (width, height) and  list of shapes to put in that region
type Region = (Point, [Index])

type ParsedType = ([Shape], [Region])

rotateRight :: Shape -> Shape
rotateRight xs = V.map f xs where
 f (0,0) = (1,0) 
 f (1,0) = (2,0)
 f (2,0) = (2,1)
 f (0,1) = (0,0) 
 f (1,1) = (1,1)
 f (2,1) = (2,2)
 f (0,2) = (0,1) 
 f (1,2) = (0,2)
 f (2,2) = (1,2)
 f x = error $ "rotateRight doesn't accept input " <> show x

flipVertical :: Shape -> Shape
flipVertical xs = V.map f xs where
 f (0,0) = (2,0) 
 f (1,0) = (1,0)
 f (2,0) = (0,0)
 f (0,1) = (2,1) 
 f (1,1) = (1,1)
 f (2,1) = (0,1)
 f (0,2) = (2,2) 
 f (1,2) = (1,2)
 f (2,2) = (0,2)
 f x = error $ "flipVertical doesn't accept input " <> show x

flipHorizontal :: Shape -> Shape
flipHorizontal xs = V.map f xs where
 f (0,0) = (0,2) 
 f (1,0) = (1,2)
 f (2,0) = (2,2)
 f (0,1) = (0,1) 
 f (1,1) = (1,1)
 f (2,1) = (2,1)
 f (0,2) = (0,0) 
 f (1,2) = (1,0)
 f (2,2) = (2,0)
 f x = error $ "rotateRight doesn't accept input " <> show x

createEmptyRegion width height = V.replicate (width*height) False 

-- |
-- For each region we try to make it work. We then count the number of regions that work.
-- For each region we generate all ways we can put the shapes in the region and check if any works.
part1 :: ParsedType -> Int
part1 (shapes, regions) = length $ filter id regionChecks where
  regionChecks = fmap f regions
  f region@(widthHeight, indexes) =  any (checkListRegions widthHeight) $  createPossibilities shapes region

-- Preciso de criar função que gera todas as possibilidades de colocar n0 formas 0, n1 formas1, etc. com todas as rotações, e inversões e posicionamentos possíveis.
-- take each shape, associate with a point.
createPossibilities :: [Shape] -> Region -> [[(Point, Shape)]]
createPossibilities = undefined

checkListRegions :: Point -> [(Point, Shape)] -> Bool
checkListRegions (width, height) xs = go (createEmptyRegion width height) xs where
  go region [] = True
  go region ((p,shape):xs) = case maybeNewRegion of
      Nothing -> False
      Just newRegion -> go newRegion xs
    where
      maybeNewRegion = putShapeInRegion width height region p shape

putShapeInRegion :: Int -> Int -> Area -> Point -> Shape -> Maybe Area
putShapeInRegion width height area topLeft@(x,y) shape
  | (x + 3 > width) || (y + 3 > height) = Nothing -- should be error ?
  | shapeInAreaIsOccupied width area topLeft shape = Nothing
  | otherwise = Just $ setShapeInArea width area topLeft shape

shapeInAreaIsOccupied :: Int -> Area -> Point -> Shape -> Bool
shapeInAreaIsOccupied width area topLeft shape = V.any f shape where
  f p = area V.! (convertIndex width $ addPoint topLeft p)

setShapeInArea :: Int -> Area -> Point -> Shape -> Area
setShapeInArea width area topLeft shape = V.update area values where
  values = V.map f shape
  f p = (convertIndex width $ addPoint topLeft p, True)

-- Is this really slow ?
-- putShapeInRegion :: Int -> Int -> Shape -> Point -> Shape -> Maybe Shape
-- putShapeInRegion width height region (x,y) shape
--   | (x + 3 > width) || (y + 3 > height) = Nothing
--   | not $ Set.disjoint region shapeTranslated = Nothing
--   | otherwise = Just $ Set.union region shapeTranslated
--   where
--     shapeTranslated = fmap (addPoint p) shape

-- could be optimized by geting row only once
-- anyTrueSubregion v (x,y) = any id values where 
--   positions = do
--     i <- [0..2]
--     j <- [0..2]
--     return (x + i , y + j)
--   values = fmap (\pos -> shapeAt pos v) positions

addPoint :: Point -> Point -> Point
addPoint (a,b) (c,d) = (a+c,b+d)

--shapeAt :: Point -> Shape -> Bool
--shapeAt (x,y) v = (v V.! y) V.! x

pNumber :: forall a. Read a => Parser a
pNumber = read <$> some digitChar

listImap :: (Int -> a -> b) -> [a] -> [b]
listImap f xs = fmap (uncurry f) $ zip [0..] xs

showShape :: Shape -> String
showShape xs = undefined 

parserShape :: Parser Shape
parserShape = do
  pNumber @Int
  char ':'
  newline
  xs <- replicateM 3 $ do
    ys <- replicateM 3 $ choice [char '#' $> True, char '.' $> False]
    newline
    return ys
  newline
  let zs = listImap (\j ys' -> listImap (\i b -> if b then Just (i,j) else Nothing) ys') xs
  return $ V.fromList $ catMaybes $ concat zs

parserRegion :: Parser Region
parserRegion = do
  a <- pNumber @Int
  char 'x'
  b <- pNumber @Int
  string ": "
  nums <- sepBy1 (pNumber @Int) (char ' ')
  return ((a,b) , nums)

parser :: Parser ParsedType
parser = do
  shapes <- some (try parserShape)
  regions <- some parserRegion
  return (shapes, regions)



part2 = undefined

-- Tests

genTestString :: IO ()
genTestString = do
    s <- readFile "test_input"
    putStrLn $  "tString = " <> show s

tString = "0:\n###\n##.\n##.\n\n1:\n###\n##.\n.##\n\n2:\n.##\n###\n##.\n\n3:\n##.\n###\n##.\n\n4:\n###\n#..\n###\n\n5:\n###\n.#.\n###\n\n4x4: 0 0 0 0 2 0\n12x5: 1 0 1 0 2 2\n12x5: 1 0 1 0 3 2\n"

tParsed = case parse parser "input" tString of
    Right x -> x
    Left _ -> error "not parsed"

testFold1 = foldr' f 0 where
  f element 10 = 10
  f element n = n + 1

testFold2 = testFold1 [1,2,3,4,5,6,7,8,9,10,undefined]

-- this loops
tFold3 = testFold1 [1..]

tFold4 10 xs = 10
tFold4 n [] = n
tFold4 n (x:xs) = tFold4 (n+1) xs

tFold5 = tFold4 0 [1..3]

-- nice, short circuit accomplished ! I just have to fuse the f with foldr and shortcircuit when neeed.
tFold6 = tFold4 0 [1..]



-- >>> OnePerLine tParsed
-- not parsed

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
