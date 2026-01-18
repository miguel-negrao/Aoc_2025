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
notes: knowing sepEndBy and between, parsing becomes easy.

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
import Control.Lens
import Data.Maybe (catMaybes, fromMaybe)
import Control.Comonad.Env (EnvT(..), ask)
import Control.Monad (guard)
import Control.Comonad.Trans.Env (runEnvT)
import qualified Data.Foldable as Set
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

data LightStatus = On | Off deriving (Show, Eq)
data Machine = Machine {
  indicatorLightDiagram :: [LightStatus],
  buttonWiringSchematics :: [[Int]],
  joltageRequirements :: [Int]
} deriving (Show, Eq)

type ParsedType = [Machine]

{--
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
-}

pNumber :: forall a. Read a => Parser a
pNumber = read <$> some digitChar

charToLightStatus :: Char -> LightStatus
charToLightStatus '.' = Off
charToLightStatus '#' = On

parser :: Parser ParsedType
parser = some $ do
  indicatorLightDiagram <- fmap charToLightStatus <$> between (char '[') (char ']') (some (choice [char '.', char '#']))
  char ' '
  buttonWiringSchematics <- between (char '(') (char ')') (pNumber @Int `sepBy` char ',') `sepEndBy` char ' '
  joltageRequirements <- between (char '{') (char '}') $ (pNumber @Int) `sepBy` char ','
  return $ Machine {..}

data Tree a = Leaf a | Tree a (Seq (Tree a)) deriving (Show, Eq)

bfsStopAt :: (a -> Bool) -> Tree a -> Maybe a
bfsStopAt pred tree = go pred (Seq.singleton tree) where
  go :: (a -> Bool) -> Seq (Tree a) -> Maybe a
  go pred (Seq.viewl -> Seq.EmptyL) = Nothing
  go pred (Seq.viewl -> (Leaf a) Seq.:< xs)
    | pred a = Just a
    | otherwise = go pred xs
  go pred (Seq.viewl -> (Tree a ys) Seq.:< xs)
    | pred a    = Just a
    | otherwise = go pred (xs Seq.>< ys) 

-- walkWithPath pred tree = go pred tree [] where
--   go pred (Leaf a) [] =
--   go pred (Tree a ys) [] =
--   go pred (Leaf a) (x:xs) =
--   go pred (Tree a ys) (x:xs) =



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
