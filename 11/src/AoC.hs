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
attempts: 1
used chatgpt:
notes: After reading seems to be about directed graphs. I believe the problem statement is already in the format Map a [a], where each vertice states to where it connects (adjacency list). Implemented a dfs algorithm, fairly easy. 

part2
time:
attempts: 1
used chatgpt: yes, to suggest a more efficient representation than IntMap. It suggested Vector (Vector Int).  I was stuck on taking to long, asked chatgpt for hint. She said "Even on an acyclic graph, enumerating/counting paths with plain DFS can take extremely long when many branches recombine, because the same subproblems get revisited many times." From there I assumed I needed memoization, and went to my usual solution with data-memocombinators.
notes: initial attemp too slow.  Did a version with IntMap and another with Vector (Vector Int). After adding memoization IntMap is faster.

Benchmark bench: RUNNING...
All
  part1 without parsing:           OK
    6.10 ms ± 304 μs
  part2 without parsing p2_intmap: OK
    13.2 ms ± 1.1 ms
  part2 without parsing p2_vector: OK
    16.7 ms ± 1.4 ms
  part1 with parsing:              OK
    7.63 ms ± 567 μs
  part2 with parsing p2_intmap:    OK
    15.6 ms ± 117 μs
  part2 with parsing p2_vector:    OK
    22.5 ms ± 1.7 ms

All 6 tests passed (5.47s)
--}

module AoC
    ( Parser
    , parser
    , part1
    , part2
    , convertGraph2
    , convertGraph2'
    , dft2_1
    , dft2_2
    , svr
    , ParsedType
    ) where

import Data.List
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad
import Data.Foldable
import Data.Function (fix)
import Text.Megaparsec.Debug
import Data.Ord (Down(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Strict as V
import qualified Data.MemoCombinators as Memo

-- by ChatGPT
newtype OnePerLine a = OnePerLine [a]

-- by ChatGPT
instance Show a => Show (OnePerLine a) where
  show :: Show a => OnePerLine a -> String
  show (OnePerLine xs) = unlines (map show xs)

type Parser = Parsec Void Text

type ParsedType = Map String [String]

{--
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
-}

pNumber :: forall a. Read a => Parser a
pNumber = read <$> some digitChar

pLabel :: Parser String
pLabel = Control.Monad.replicateM 3 alphaNumChar

pLine :: Parser (String, [String])
pLine = do
  device <- pLabel
  string ": "
  outputs <- sepBy1 pLabel (char ' ')
  newline
  return (device, outputs)

parser :: Parser ParsedType
parser = Map.fromList <$> some pLine

type Graph = IntMap [Int]
type VectorGraph = V.Vector (V.Vector Int)

you = 0

convertGraph :: [String] -> Map String [String] -> IntMap [Int]
convertGraph important map = IntMap.fromList xs where
  allStrings = nub $ Map.keys map ++ concat (Map.elems map)
  allStrings' = important ++ (allStrings \\ important)
  ys = Map.toList map
  xs = fmap f ys
  f (a, zs) = (indexOf a, fmap indexOf zs)
  indexOf x = fromMaybe (error "elem not in array") $ elemIndex x allStrings'

convertGraph1 :: Map String [String] -> IntMap [Int]
convertGraph1 map = convertGraph ["you", "out", "dac", "fft", "svr"] map

dac = 1
fft = 2
svr = 3

convertGraph2 :: Map String [String] -> IntMap [Int]
convertGraph2 map = convertGraph ["out", "dac", "fft", "svr"] map

convertGraph2' :: Map String [String] -> VectorGraph
convertGraph2' map = v where
  allStrings = nub $ Map.keys map ++ concat (Map.elems map)
  important = ["out", "dac", "fft", "svr"]
  allStrings' = important ++ (allStrings \\ important)
  indexOf x = fromMaybe (error "elem not in array") $ elemIndex x allStrings'
  v = V.fromList $ fmap f $ [0..(length allStrings' - 1)]
  f n = V.fromList $ fmap indexOf $ fromMaybe [] $ Map.lookup (allStrings' !! n) map

lookupGraph :: Int -> Graph -> [Int]
lookupGraph elem g = fromMaybe [] (IntMap.lookup elem g)

lookupGraph' :: Int -> IntMap (Seq Int) -> Seq Int
lookupGraph' elem g = fromMaybe Seq.empty (IntMap.lookup elem g)

dft :: Int -> Graph -> Int
dft start g = go start [] where
  go :: Int -> [Int] -> Int
  go 1 _ = 1
  go element seenAlready
      | element `elem` seenAlready = 0
      | otherwise = sum $ fmap f (lookupGraph element g) where
          f newElement = go newElement (element:seenAlready)

part1 :: ParsedType -> Int
part1 = dft you . convertGraph1

-- Assume no cycles
-- this runs ok 12.8 ms
dft2_1 :: Int -> Graph -> Int
dft2_1 start g = go start 0 where
  go = Memo.memo2 Memo.integral Memo.integral go'
  go' :: Int -> Int -> Int
  go' 0 n 
    | n >= 2 = 1
    | otherwise = 0
  go' element n
      | otherwise = sum $ fmap f nodes where
          nodes = lookupGraph element g
          f newElement = go newElement n'
          n' = if element == dac || element == fft then n + 1 else n -- trace ("found " <> show element)

-- Assume no cycles
-- 16.1 ms
dft2_2 :: Int -> VectorGraph -> Int
dft2_2 start g = go start 0 where
  go = Memo.memo2 Memo.integral Memo.integral go'
  go' :: Int -> Int -> Int
  go' 0 n
    | n >= 2 = 1
    | otherwise = 0
  go' element n
      | otherwise = sum $ fmap f nodes where
          nodes = g V.! element
          f newElement = go newElement n'
          n' = if element == dac || element == fft then n + 1 else n -- trace ("found " <> show element) 

part2 :: ParsedType -> Int
part2 = dft2_1 svr . convertGraph2














-- Tests

genTestString :: IO ()
genTestString = do
    s <- readFile "test_input2"
    putStrLn $  "tString = " <> show s

tString = "aaa: you hhh\nyou: bbb ccc\nbbb: ddd eee\nccc: ddd eee fff\nddd: ggg\neee: out\nfff: out\nggg: out\nhhh: ccc fff iii\niii: out\n"

tParsed = case parse parser "input" tString of
    Right x -> x
    Left _ -> error "not parsed"

-- >>> tParsed
-- fromList [("aaa",["you","hhh"]),("bbb",["ddd","eee"]),("ccc",["ddd","eee","fff"]),("ddd",["ggg"]),("eee",["out"]),("fff",["out"]),("ggg",["out"]),("hhh",["ccc","fff","iii"]),("iii",["out"]),("you",["bbb","ccc"])]

tString2 = "svr: aaa bbb\naaa: fft\nfft: ccc\nbbb: tty\ntty: ccc\nccc: ddd eee\nddd: hub\nhub: fff\neee: dac\ndac: fff\nfff: ggg hhh\nggg: out\nhhh: out\n"

tParsed2 = case parse parser "input" tString2 of
    Right x -> x
    Left _ -> error "not parsed"

tConv2 = convertGraph2 tParsed2

{--


dft2 :: Int -> Graph -> Int
dft2 start g = go start 0 [] where
  go :: Int -> Int -> [Int] -> Int
  go 0 n _ 
    | n >= 2 = 1
    | otherwise = 0
  go element n seenAlready
      | element `elem` seenAlready = 0 -- trace ("seen already: " <> show element) 0
      | otherwise = sum $ fmap f nodes where
          nodes = lookupGraph element g
          f newElement = go newElement n' (element:seenAlready)
          n' = if element == dac || element == fft then n + 1 else n -- trace ("found " <> show element) 
          
dft3 :: Int -> VectorGraph -> Int
dft3 start g = go start 0 [] where
  go :: Int -> Int -> [Int] -> Int
  go 0 n _ 
    | n >= 2 = 1
    | otherwise = 0
  go element n inStack
      | element `elem` inStack = trace ("cycle detected") 0 -- trace ("seen already: " <> show element) 0
      | otherwise = sum $ fmap f nodes where
          nodes = g V.! element
          f newElement = go newElement n' (element:inStack)
          n' = if element == dac || element == fft then n + 1 else n -- trace ("found " <> show element) 

bft2 :: Int -> Graph -> Int
bft2 start g = go (Seq.singleton (0,start)) 0 [] where
  g' :: IntMap (Seq Int)
  g' = fmap Seq.fromList g
  go :: Seq (Int,Int) -> Int -> [Int] -> Int
  go (Seq.viewl -> Seq.EmptyL) pathCount _ = pathCount 
  go (Seq.viewl -> (n,x) Seq.:< xs) pathCount seen   
    | x `elem` seen = go xs pathCount seen
    | otherwise = go xs' pathCount' seen' where
                    seen' = (x:seen)
                    xs' = xs Seq.>< fmap h nodes
                    h y = (n', y)
                    nodes = lookupGraph' x g'
                    n' = if x == dac || x == fft then n + 1 else n
                    pathCount' = if x == out && n >= 2 then pathCount + 1 else pathCount

-- Assume no cycles
bft4 :: Int -> VectorGraph -> Int
bft4 start g = go (Seq.singleton (0,start)) 0 where
  go :: Seq (Int,Int) -> Int -> Int
  go (Seq.viewl -> Seq.EmptyL) pathCount = pathCount 
  go (Seq.viewl -> (n,x) Seq.:< xs) pathCount   
    | otherwise = go xs' pathCount' where
                    xs' = xs Seq.>< fmap h nodes
                    h y = (n', y)
                    nodes = Seq.fromList $ V.toList $ g V.! x
                    n' = if x == dac || x == fft then n + 1 else n
                    pathCount' = if x == out && n >= 2 then pathCount + 1 else pathCount

--}
