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
used chatgpt: yes.
- to get some help on pinpointing the memory leak. Asked for very generic analysis of my approach, essentially if it could in theory work or not. It wasn't very clear what the answer was, I think it was, perhaphs with some changes. 
- She suggests moving to bit as Integer. My input max seems to be 50x50 that is 2500 bits, will use Integer. 
- She detected an error with my rotation, fixed that. 
- Suggested speedup by checking impossibility in number of live cells vs total cells of the grid.
notes: My initial idea was, generate all ways to put the pattens on the area and then check each one. I choose Vector Bool in 1D, I thought that would be fast enough. First attempt blows up on the test case in memory (and time). Felling stuck, chatgpt hinted at somehow fusing the generations and consumption of the array and also switching to bitfields. Once I switched to bitfield I saw this simplified a lot because a present placed in given point and rotation is just one Intenger, and checking and placing is just binary AND and OR of Integers. Still that did not fix the memory leak. For v3 I redid the list monad this time using guards to not generate any further when then first elements fail. v3 has the memory under control, but takes 213s on the test, not good, that doesn't work on the main input, will not finish ever. Suggested by ChatGPT first checked if the number of live cells even fits in the area and aborts straight away if not, with that it runs in 29s.

part1 without parsing: OK
    28.067 s ± 2.23 s
part1 with parsing:    OK
    29.365 s ± 5.4 ms

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

{--
(0,0) (1,0) (2,0)
(0,1) (1,1) (2,1)
(0,2) (1,2) (2,2)
--}

rotateRight :: Shape -> Shape
rotateRight xs = fmap f xs where
 f (0,0) = (2,0) 
 f (1,0) = (2,1)
 f (2,0) = (2,2)
 f (0,1) = (1,0) 
 f (1,1) = (1,1)
 f (2,1) = (1,2)
 f (0,2) = (0,0) 
 f (1,2) = (0,1)
 f (2,2) = (0,2)
 f x = error $ "rotateRight doesn't accept input " <> show x

flipX :: Shape -> Shape
flipX xs = fmap f xs where
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
-- I think with this version I'm generating many combinations that I would already know they don't work because the first n elements already don't work.
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

part1 :: ParsedType -> Int
part1 = part1v3

-- |
-- For each region we try to make it work. We then count the number of regions that work.
-- For each region we generate all ways we can put the shapes in the region and check if any works.
part1V1 :: ParsedType -> Int
part1V1 (shapes, regions) = length $ trace ("part1: " <> show regionChecks <> "\n") filter id regionChecks where
  regionChecks = fmap f regions
  f region@(dims, indexes) =  any checkListRegions $  createPossibilities shapes region

-- |
-- I believe this will build all areas that work starting from the last shapes to place. and then try adding one more shape. Probably still to much memory, it's not exactly this.
part1v2 :: ParsedType -> Int
part1v2 (shapes, regions) = length $ trace ("part1: " <> show regionChecks <> "\n") filter id regionChecks where
  regionChecks = fmap f regions
  f region@(dims, indexes) = not $ null $ go allPossibilites where  
    allPossibilites = createPossibilities' shapes region
    go :: [[Integer]] -> [Integer] -- keep just final patterns that work
    go [] = [0]
    go (y:ys) =
      do
        z <- y
        area <- go ys
        guard $ area .&. z == 0 
        return $ z .|. area

-- | memory usage tiny 0.1%
-- It works, but took 213s... that's not going to work on input...
part1v3 :: ParsedType -> Int
part1v3 (shapes, regions) = length $ filter id regionChecks where
  -- length $ trace ("part1: " <> show regionChecks <> "\n") filter id regionChecks where
  regionChecks = fmap f regions
  f region@(dims, indexes) = checkFits shapes region && (not $ null $ go 0 allPossibilites) where  
    allPossibilites = createPossibilities' shapes region
    go :: Integer -> [[Integer]] -> [[Integer]] -- keep just final patterns that work
    go area0 [] = [[]]
    go area0 (y:ys) =
      do
        z <- y
        guard $ area0 .&. z == 0
        zs <- go (area0 .|. z) ys
        return $ z:zs

checkFits :: [Shape] -> (Dimensions, [Index]) -> Bool
checkFits shapes ((w,h), is) = (w*h) >= n where
    n = sum $ zipWith (*) (fmap length shapes) is

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

createPossibilities' :: [[Point]] -> ((Int, Int), [Int]) -> [[Integer]]
createPossibilities' shapes (dims@(width, height), indexes) = shapes3 where
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
-- I tried drawing and seems that the only unique shapes are the original plus 3 rotations and their horizontal flips. Vertical flips will correspond to some horizontal flip. There might be duplicates, remove.
generateRotationsFlips :: Shape -> [Shape]
generateRotationsFlips shape = nub $ rotations ++ flips where
  rotations = take 4 $ iterate rotateRight shape
  flips = fmap flipX rotations

checkListRegions :: [Integer] -> Bool
checkListRegions xs = go startArea xs where
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

