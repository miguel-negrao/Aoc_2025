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
{-# LANGUAGE BangPatterns #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}

{--
part1
time: 
attempts:
used chatgpt: yes, to get some help on pinpointing the memory leak. Asked for very generic analysis of my approach, essentially if it could in theory work or not. It wasn't very clear what the answer was, I think it was, perhaphs with some changes. She suggests moving to bit as Integer. My input max seems to be 50x50 that is 2500 bits
notes: First attempt blows up in memory even in the test case. Need to make stuff less lazy ?

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
import Data.Bits

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
type Width = Int
type Height = Int
type Dimensions = (Width, Height)
type Index = Int
type Shape = [Point]
-- A flat array stored as bitset where we calculate the 2D -> 1D index conversion
type Area = Integer

convertIndex :: Int -> Point -> Int
convertIndex width (x,y) = (y*width) + x

-- (width, height) and  list of shapes to put in that region
type Region = (Dimensions, [Index])

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

flipVertical :: Shape -> Shape
flipVertical xs = fmap f xs where
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
flipHorizontal xs = fmap f xs where
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

-- |
-- Given a list of lists generate all ways of picking one element out of every list, returning them in a list in the same order as the lists that were given.
choices :: [[x]] -> [[x]]
choices xs0 = {-# SCC "choices" #-} go xs0 where
  go [] = [[]]
  go (y:ys) =
    do
      z <- y
      zs <- go ys
      return $ z:zs

-- >>> choices [[1,2],[5,6,7]]
-- [[1,5],[1,6],[1,7],[2,5],[2,6],[2,7]]

-- >>> choices [[1,2],[5,6,7],[8,9]]
-- [[1,5,8],[1,5,9],[1,6,8],[1,6,9],[1,7,8],[1,7,9],[2,5,8],[2,5,9],[2,6,8],[2,6,9],[2,7,8],[2,7,9]]

-- |
-- For each region we try to make it work. We then count the number of regions that work.
-- For each region we generate all ways we can put the shapes in the region and check if any works.
part1 :: ParsedType -> Int
part1 (shapes, regions) = length $ trace ("part1: " <> show regionChecks <> "\n") filter id regionChecks where
  regionChecks = fmap f regions
  f region@(dims, indexes) =  any (checkListRegions dims) $  createPossibilities shapes region

generateBitField :: Width -> Point -> Shape -> Integer
generateBitField width topLeft shape = foldr g 0 indexes where
  g i x = setBit x i
  indexes = fmap f shape
  f p = convertIndex width $ addPoint topLeft p

-- Preciso de criar função que gera todas as possibilidades de colocar n0 formas 0, n1 formas1, etc. com todas as rotações, e inversões e posicionamentos possíveis.
-- take each shape, associate with a point.
createPossibilities :: [Shape] -> (Dimensions, [Index]) -> [[Integer]] -- was (Point, Shape) 
createPossibilities shapes (dims@(width, height), indexes) = {-# SCC "createPossibilities" #-} choices shapes3 where
    (maxW, maxH) = addPoint dims (-3, -3)
    shapes3 = {-# SCC "createPossibilities.shapes3" #-} concat $ zipWith f shapes2 indexes
    f xs n = {-# SCC "createPossibilities.replicate" #-} replicate n xs
    shapes2 = {-# SCC "createPossibilities.shapes2" #-} fmap g shapes
    genBitField2 = generateBitField width
    g shape = {-# SCC "createPossibilities.shapeOptions" #-} do
      p <- positions
      s <- generateRotationsFlips shape
      return $ genBitField2 p s
    positions = {-# SCC "createPossibilities.positions" #-} do
      x <- [0..maxW]
      y <- [0..maxH]
      return (x,y)


-- |
-- generate all rotations and flips of this shape
-- I tried drawing and seems that the only unique shapes are the original plus 3 rotations and their horizontal flips. Vertical flips will correspond to some horizontal flip.
generateRotationsFlips :: Shape -> [Shape]
generateRotationsFlips shape = rotations ++ flips where
  rotations = take 4 $ iterate rotateRight shape
  flips = fmap flipHorizontal rotations

checkListRegions :: Dimensions -> [Integer] -> Bool
checkListRegions dims xs = go startArea xs where
  startArea = 0
  go _ [] = True
  go area (x:xs) = if area .&. x == 0 then go (area .|. x) xs else False

addPoint :: Point -> Point -> Point
addPoint (a,b) (c,d) = (a+c,b+d)

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
  return $ catMaybes $ concat zs

parserRegion :: Parser Region
parserRegion = do
  a <- pNumber @Int
  char 'x'
  b <- pNumber @Int
  string ": "
  nums <- sepBy1 (pNumber @Int) (char ' ')
  newline
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

-- >>> tParsed
-- ([[(0,0),(1,0),(2,0),(0,1),(1,1),(0,2),(1,2)],[(0,0),(1,0),(2,0),(0,1),(1,1),(1,2),(2,2)],[(1,0),(2,0),(0,1),(1,1),(2,1),(0,2),(1,2)],[(0,0),(1,0),(0,1),(1,1),(2,1),(0,2),(1,2)],[(0,0),(1,0),(2,0),(0,1),(0,2),(1,2),(2,2)],[(0,0),(1,0),(2,0),(1,1),(0,2),(1,2),(2,2)]],[((4,4),[0,0,0,0,2,0]),((12,5),[1,0,1,0,2,2]),((12,5),[1,0,1,0,3,2])])

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



--shapeAt :: Point -> Shape -> Bool
--shapeAt (x,y) v = (v V.! y) V.! x
