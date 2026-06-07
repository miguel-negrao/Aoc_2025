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
used chatgpt: yes: get documentation of Seq quickly; to find equivalent of concat for Seq.
notes: knowing sepEndBy and between, parsing becomes easy. Initial idea is to form a Tree of choices of button pressing and then travel the tree in breadth-first keeping track of the node traveled to get to each node, and stop once the sequence gives the necessary result. 5.631 s ± 746 ms is a bit too slow.
V2 keeps intermediate results in tree
V3 uses bit field, now that is much better

v1
  part1 without parsing: OK
    5.975 s ±  12 ms
  part1 with parsing:    OK
    6.183 s ± 378 ms
v2 keep state
  part1 without parsing: OK
    4.311 s ±  12 ms
  part1 with parsing:    OK
    4.311 s ±  21 ms
v3 bit field
  part1 without parsing: OK
    3.250 s ± 102 ms
  part1 with parsing:    OK
    3.133 s ± 123 ms

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
import Control.Monad (guard)
import Control.Comonad.Trans.Env (runEnvT)
import Data.Foldable
import Data.Function (fix)
import Text.Megaparsec.Debug
--import Linear.V3
--import Linear.Metric
import Math.Combinat.Sets (combine, choose)
import Data.Ord (Down(..))
import Data.Bits

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

type ParsedType = [Machine]

{--
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
-}

pNumber :: forall a. Read a => Parser a
pNumber = read <$> some digitChar

charToLightStatus :: Char -> Bool
charToLightStatus '.' = False
charToLightStatus '#' = True

parser :: Parser ParsedType
parser = some $ do
  indicatorLightDiagram <- fmap charToLightStatus <$> between (char '[') (char ']') (some (choice [char '.', char '#']))
  char ' '
  buttonWiringSchematics <- between (char '(') (char ')') (pNumber @Int `sepBy` char ',') `sepEndBy` char ' '
  joltageRequirements <- between (char '{') (char '}') $ (pNumber @Int) `sepBy` char ','
  newline
  return $ Machine {..}

data Tree a = Tree a (Seq (Tree a)) deriving (Show, Eq)

-- |
-- To see the execution of the function in the Haskell debugger I set breakpoints in the Leaf a and Tree a cases.
bfsStopAt :: (a -> Bool) -> Tree a -> Maybe a
bfsStopAt pred tree = go pred (Seq.singleton tree) where
  go :: (a -> Bool) -> Seq (Tree a) -> Maybe a
  go pred (Seq.viewl -> Seq.EmptyL) = Nothing
  -- go pred (Seq.viewl -> (Leaf a) Seq.:< xs)
  --   | pred a = Just a
  --   | otherwise = go pred xs
  go pred (Seq.viewl -> (Tree a ys) Seq.:< xs)
    | pred a    = Just a
    | otherwise = go pred (xs Seq.>< ys)

bfsStopAtPath :: forall a. Show a => Int -> ((Seq a) -> Bool) -> Tree a -> Maybe (Seq a)
bfsStopAtPath max pred tree = go 0 pred $ Seq.singleton (Seq.Empty, tree) where
  go :: Int -> (Seq a -> Bool) -> Seq (Seq a, Tree a) -> Maybe (Seq a)
  -- No more nodes to process, stop
  go _ pred (Seq.viewl -> Seq.EmptyL) = Nothing
  go n pred (Seq.viewl -> (zs, Tree a ys) Seq.:< xs)
    | n > max = Nothing
    -- found element, stop
    | pred (zs Seq.|> a) = Just $ zs Seq.|> a
    -- add all branches of this tree to the list of nodes to process
    | otherwise = go (n + 1) pred $ xs Seq.>< (fmap (\b -> (zs Seq.|> a, b)) ys)


part1 = part1v1

part1v1 :: ParsedType -> Int
part1v1 xs = (sum smallestSets) where -- trace ("smallestSets = " <> show smallestSets) 
  smallestSets = fmap processMachineV1 xs

processMachineV1 (Machine pattern buttons _) = case res of
      Just ys -> Seq.length ys - 1 -- trace ("ys = " <> show ys) $ 
      Nothing -> error "part1 cannot find solution"
    where 
      res = bfsStopAtPath 100000000 pred $ part1BuildList Seq.empty buttons'
      buttons' = Seq.fromList $ fmap Seq.fromList buttons
      startPattern = Seq.replicate (length pattern) False
      seqPattern = Seq.fromList pattern
      pred xs = applyButtons startPattern xs == seqPattern

-- I love lazy data structures !!
-- | this builds a tree like bi are buttons
--            []
--      b1              b2            ...    bn      
--b1 b2 ..bn       b1 b2... bn            b1 b2 ... bn  
--                      ...
part1BuildList :: a -> Seq a -> Tree a
part1BuildList init xs = Tree init $ go xs where
  go xs = fmap f xs where
    f x = Tree x $ go xs

applyButtons :: Seq Bool -> Seq (Seq Int) -> Seq Bool
applyButtons xs ys = foldr f xs (fold ys) where
  f = Seq.adjust' not

-- V2 Keep just intermediate results while traversing the tree

part1v2 :: ParsedType -> Int
part1v2 xs = (sum smallestSets) where -- trace ("smallestSets = " <> show smallestSets) 
  smallestSets = fmap processMachineV2 xs

applyButton :: Seq Bool -> Seq Int -> Seq Bool
applyButton pattern button = foldr (Seq.adjust' not) pattern button

processMachineV2 (Machine pattern buttons _) =  case res of
      Just m -> m -- trace ("ys = " <> show ys) $ 
      Nothing -> error "part1 cannot find solution"
    where
      startPattern = Seq.replicate (length pattern) False 
      buttons' = Seq.fromList $ fmap Seq.fromList buttons
      res = bfsStopAtPath2 startPattern 100000000 f $ part1BuildList Seq.empty buttons'
      seqPattern = Seq.fromList pattern
      f pattern button = let b' = foldr (Seq.adjust' not) pattern button in (b', b' == seqPattern)

bfsFoldStop :: forall a b. Show a => b -> Int -> (b -> a -> (b, Bool)) -> Tree a -> Maybe Int
bfsFoldStop init max f tree = go 0 $ Seq.singleton (init, 0, tree) where
  go :: Int -> Seq (b, Int, Tree a) -> Maybe Int
  -- No more nodes to process, stop
  go n (Seq.viewl -> Seq.EmptyL) = Nothing
  go n (Seq.viewl -> (b, m, Tree a ys) Seq.:< xs)
    | n > max = Nothing
    -- found element, stop
    | stop = Just m
    -- add all branches of this tree to the list of nodes to process
    | otherwise = go (n + 1) $ xs Seq.>< (fmap (\y -> (b', m+1, y)) ys)
    where
      (b', stop) = f b a

part1v3 :: ParsedType -> Int
part1v3 xs = (sum smallestSets) where -- trace ("smallestSets = " <> show smallestSets) 
  smallestSets = fmap processMachineV3 xs

processMachineV3 (Machine pattern buttons _) =  case res of
      Just m -> m -- trace ("ys = " <> show ys) $ 
      Nothing -> error "part1 cannot find solution"
    where
      startPattern = 0
      buttons' :: Seq Integer
      buttons' = Seq.fromList $ fmap toInteger buttons
      toInteger :: [Int] -> Integer
      toInteger = foldr (flip setBit) 0
      res = bfsFoldStop startPattern 100000000 f $ part1BuildList 0 buttons'
      intPattern =  foldr (\(i, isOn) x -> if isOn then setBit x i else clearBit x i) 0 $ zip [0..] pattern
      f pattern button = let b' = pattern `xor` button in (b', b' == intPattern)

-- | right now test doesn't even find the right answer and need to check if I'm keeping the intermediate calculations
part2 :: ParsedType -> Int
part2 machines = sum $ fmap part2MinimumNumberOfButtonPresses machines

part2MinimumNumberOfButtonPresses :: Machine -> Int
part2MinimumNumberOfButtonPresses (Machine _ buttons joltages) = case a of
      Just ys -> Seq.length ys - 1 -- trace ("ys = " <> show ys) $ 
      Nothing -> error "part1 cannot find solution"
    where
      startPattern = Seq.replicate (length joltages) 0
      buttons' = Seq.fromList $ (fmap Seq.fromList) buttons
      a = bfsStopAtPath 1000 pred $ part2BuildList buttons'
      seqJoltages = Seq.fromList joltages
      pred xs = applyButtonsPart2 startPattern xs == seqJoltages

part2BuildList :: Seq (Seq a) -> Tree (Seq a)
part2BuildList xs = Tree Seq.empty $ go xs where
  go xs = fmap f xs where
    f x = Tree x $ go xs

applyButtonsPart2 :: Seq Int -> Seq (Seq Int) -> Seq Int
applyButtonsPart2 startJoltages buttons = foldr f startJoltages buttons where
  f button joltages = foldr sumAt joltages button
  sumAt i counters = Seq.adjust' (+1) i counters

-- Tests

genTestString :: IO ()
genTestString = do
    s <- readFile "test_input"
    putStrLn $  "tString = " <> show s

tString = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}\n"

tParsed = case parse parser "input" tString of
    Right x -> x
    Left _ -> error "not parsed"

tTest1 = part1 tParsed

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
