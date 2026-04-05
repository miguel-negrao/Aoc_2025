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
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Foldable
import Data.Function (fix)
import Text.Megaparsec.Debug
import Data.Vector (Vector)
import qualified Data.Vector as V
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
type Shape = Set Point
-- (width, height) and  list of shapes to put in that region
type Region = (Point, [Index])

type ParsedType = ([Shape], [Region])

rotateRight :: Shape -> Shape
rotateRight xs = fmap f xs where
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
 

checkListRegions :: Point -> [(Point, Shape)] -> Bool
checkListRegions (width, height) xs = go (createEmptyRegion width height) xs where
  go region [] = True
  go region (x:xs) = undefined

-- Is this really slow ?
putShapeInRegion :: Int -> Int -> Shape -> Point -> Shape -> Maybe Shape
putShapeInRegion width height region (x,y) shape
  | (x + 3 > width) || (y + 3 > height) = Nothing
  | not $ Set.disjoint region shapeTranslated = Nothing
  | otherwise = Just $ Set.union region shapeTranslated
  where
    shapeTranslated = fmap (addPoint p) shape

-- could be optimized by geting row only once
-- anyTrueSubregion v (x,y) = any id values where 
--   positions = do
--     i <- [0..2]
--     j <- [0..2]
--     return (x + i , y + j)
--   values = fmap (\pos -> shapeAt pos v) positions

addPoint :: Point -> Point -> Point
addPoint (a,b) (c,d) = (a+c,b+d)

shapeAt :: Point -> Shape -> Bool
shapeAt (x,y) v = (v V.! y) V.! x

updateArea :: Area -> Shape -> Area
updateArea = 

pNumber :: forall a. Read a => Parser a
pNumber = read <$> some digitChar

parserShape :: Parser Shape
parserShape = do
  pNumber @Int
  char ':'
  newline
  x <- V.replicateM 3 $ do
    y <- V.replicateM 3 $ choice [char '#' $> True, char '.' $> False]
    newline
    return y
  newline
  return x 

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

createEmptyRegion :: Int -> Int -> Shape
createEmptyRegion w h = V.replicate h (V.replicate w False)

addPresent :: Point -> Shape -> Point -> Shape -> Maybe Shape
addPresent (w,h) region (x,y) present
  | (x + V.length (present V.! 0) > w) || 
    (y + V.length present > h) = Nothing

checkPresents :: [Shape] -> Point -> [(Index, Point)]
checkPresents = undefined

part1 :: ParsedType -> Int
part1 xs = undefined

part2 = undefined

-- Tests

genTestString :: IO ()
genTestString = do
    s <- readFile "test_input"
    putStrLn $  "tString = " <> show s

tString = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}\n"

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
