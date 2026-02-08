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
used chatgpt:
notes: After reading seems to be about directed graphs. I believe the problem statement is already in the format Map a [a], where each vertice states to where it connects.

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
import Control.Comonad
import Control.Comonad.Store
import qualified Data.MemoCombinators as Memo
import Data.MemoCombinators (Memo)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Control.Comonad.Env (EnvT(..), ask)
import Control.Monad
import Control.Comonad.Trans.Env (runEnvT)
import Data.Foldable
import Data.Function (fix)
import Text.Megaparsec.Debug
--import Linear.V3
--import Linear.Metric
import Math.Combinat.Sets (combine, choose)
import Data.Ord (Down(..))

-- by ChatGPT
newtype OnePerLine a = OnePerLine [a]

-- by ChatGPT
instance Show a => Show (OnePerLine a) where
  show :: Show a => OnePerLine a -> String
  show (OnePerLine xs) = unlines (map show xs)

type Parser = Parsec Void Text
type LightStatus = Bool
data Machine = Machine {
  indicatorLightDiagram :: [Bool],
  buttonWiringSchematics :: [[Int]],
  joltageRequirements :: [Int]
} deriving (Show, Eq)

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

type Graph = Map Int [Int]

you = 0
out = 1

convertGraph :: Map String [String] -> Map Int [Int]
convertGraph map = Map.fromList xs where
  allStrings = nub $ Map.keys map ++ concat (Map.elems map)
  important = ["you", "out"]
  allStrings' = important ++ (allStrings \\ important)
  ys = Map.toList map
  xs = fmap f ys
  f (a, zs) = (indexOf a, fmap indexOf zs)
  indexOf x = fromMaybe (error "elem not in array") $ elemIndex x allStrings'

lookupGraph :: Int -> Graph -> [Int]
lookupGraph elem g = fromMaybe [] (Map.lookup elem g)

dft :: Int -> Graph -> Int
dft start g = go 0 [] where
  go :: Int -> [Int] -> Int
  go 1 _ = 1
  go element seenAlready
      | element `elem` seenAlready = 0
      | otherwise = sum $ fmap f (lookupGraph element g) where
          f newElement = go newElement (element:seenAlready)

part1 :: ParsedType -> Int
part1 = dft you . convertGraph

part2 :: ParsedType -> Int
part2 = undefined

-- Tests

genTestString :: IO ()
genTestString = do
    s <- readFile "test_input"
    putStrLn $  "tString = " <> show s

tString = "aaa: you hhh\nyou: bbb ccc\nbbb: ddd eee\nccc: ddd eee fff\nddd: ggg\neee: out\nfff: out\nggg: out\nhhh: ccc fff iii\niii: out\n"

tParsed = case parse parser "input" tString of
    Right x -> x
    Left _ -> error "not parsed"

-- >>> tParsed
-- fromList [("aaa",["you","hhh"]),("bbb",["ddd","eee"]),("ccc",["ddd","eee","fff"]),("ddd",["ggg"]),("eee",["out"]),("fff",["out"]),("ggg",["out"]),("hhh",["ccc","fff","iii"]),("iii",["out"]),("you",["bbb","ccc"])]