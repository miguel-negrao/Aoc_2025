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

-- by ChatGPT
newtype OnePerLine a = OnePerLine [a]

-- by ChatGPT
instance Show a => Show (OnePerLine a) where
  show :: Show a => OnePerLine a -> String
  show (OnePerLine xs) = unlines (map show xs)

type Parser = Parsec Void Text
type Point = (Int,Int)
type Index = Int
type Shape = Vector (Vector Bool)
type Region = (Point, [Index])

type ParsedType = ([Shape], [Region])

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
