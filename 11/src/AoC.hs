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
attempts: 1
used chatgpt:
notes: After reading seems to be about directed graphs. I believe the problem statement is already in the format Map a [a], where each vertice states to where it connects (adjacency list). Implemented a dfs algorithm, fairly easy. 

part2
time:
attempts: 1
used chatgpt: yes, to suggest a more efficient representation than IntMap. It suggested Vector (Vector Int).  I was stuck on taking to long, asked chatgpt for hint. She said "Even on an acyclic graph, enumerating/counting paths with plain DFS can take extremely long when many branches recombine, because the same subproblems get revisited many times." From there I assumed I needed memoization, and went to my usual solution with data-memocombinators.
notes: initial attemp too slow, memoization solved the issue.  Using Map in convertGraph makes it much faster on the lookup from string to int. Did a version with IntMap and another with Vector (Vector Int). IntMap is slightly faster, and the code is simpler. 

Benchmark bench: RUNNING...
All
  part1 without parsing:           OK
    581  μs ±  45 μs
  part2 without parsing p2_intmap: OK
    2.68 ms ±  86 μs
  part2 without parsing p2_vector: OK
    2.91 ms ±  21 μs
  part1 with parsing:              OK
    2.43 ms ± 231 μs
  part2 with parsing p2_intmap:    OK
    5.53 ms ± 134 μs
  part2 with parsing p2_vector:    OK
    5.11 ms ± 358 μs

All 6 tests passed (19.85s)
--}

module AoC
    ( Parser
    , parser
    , part1
    , part2
    , part2IntMap
    , part2Vector
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
import qualified Data.MemoCombinators.Class as Memo
import qualified Data.IntMap as M
import qualified Control.Applicative as Vector

-- by ChatGPT
newtype OnePerLine a = OnePerLine [a]

-- by ChatGPT
instance Show a => Show (OnePerLine a) where
  show :: Show a => OnePerLine a -> String
  show (OnePerLine xs) = unlines (map show xs)

type Parser = Parsec Void Text

type ParsedType = [(String, [String])]

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
parser = some pLine

type Graph = IntMap [Int]
type VectorGraph = V.Vector (V.Vector Int)

you = 0

-- todo: add binary search
-- memoize 

convertGraph :: [String] -> ParsedType -> IntMap [Int]
convertGraph important xs = IntMap.fromList ys where
  allStrings :: [String]
  -- lets assume all nodes have a line. If they don't we get an error down at indexOf. out doesn't but we add it.
  allStrings = fmap fst xs
  allStrings' :: [String]
  allStrings' = important ++ (allStrings \\ important)
  map = Map.fromList $ zipWith (\a b -> (a,b)) allStrings' [0..]
  ys = fmap f xs
  f (a, zs) = (indexOf a, fmap indexOf zs)
  indexOf :: String -> Int
  indexOf s = fromMaybe (error "elem not in array") $ Map.lookup s map


convertGraph1 :: ParsedType -> IntMap [Int]
convertGraph1 = convertGraph ["you", "out", "dac", "fft", "svr"]

dac = 1
fft = 2
svr = 3

convertGraph2 :: ParsedType -> IntMap [Int]
convertGraph2 = convertGraph ["out", "dac", "fft", "svr"]

convertGraphVector :: ParsedType -> (VectorGraph, Int, Int, Int)
convertGraphVector xs = (V.fromList ys, indexOf "svr", indexOf "dac", indexOf "fft") where
  allStrings :: [String]
  allStrings = fmap fst xs
  map' :: Map String Int
  map' = Map.fromList $ zipWith (\a b -> (a,b)) allStrings [1..]
  map :: Map String Int
  map = Map.insert "out" 0 map'
  indexOf :: String -> Int
  indexOf s = fromMaybe (error $ "elem not in array: " <> show s) $ Map.lookup s map
  ys = V.empty : fmap f xs
  f (a, zs) = V.fromList $ fmap indexOf zs

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
-- 2.68 ms
dft2_1 :: Int -> Graph -> Int
dft2_1 start g = go start 0 where
  go = Memo.memo2 Memo.integral Memo.integral go'
  go' :: Int -> Int -> Int
  go' 0 n 
    | n >= 2 = 1
    | otherwise = 0
  go' !element !n
      | otherwise = sum $ fmap f nodes where
          nodes = lookupGraph element g
          f newElement = go newElement n'
          !n' = if element == dac || element == fft then n + 1 else n -- trace ("found " <> show element)

-- Assume no cycles
-- 2.91 ms
dft2_2 :: Int -> Int -> Int -> VectorGraph -> Int
dft2_2 start dac' fft' g = go start 0 where
  go = Memo.memo2 Memo.integral Memo.integral go'
  go' :: Int -> Int -> Int
  go' 0 n 
    | n >= 2 = 1
    | otherwise = 0
  go' !element !n = sum $ fmap f nodes where
        nodes = g V.! element
        f newElement = go newElement n'
        !n' = if element == dac' || element == fft' then n + 1 else n

part2IntMap :: ParsedType -> Int
part2IntMap = dft2_1 svr . convertGraph2

part2Vector :: ParsedType -> Int
part2Vector xs = dft2_2 svr' dac' fft' g where
  (g, svr', dac', fft') = convertGraphVector xs

part2 = part2Vector












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
