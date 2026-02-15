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
used chatgpt: yes: get documentation of Seq quickly;
notes: knowing sepEndBy and between, parsing becomes easy. Initial idea is to form a Tree of choices of button pressing and then travel the tree in breadth-first keeping track of the node traveled to get to each node, and stop once the sequence gives the necessary result. 

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

data Tree a = Leaf a | Tree a (Seq (Tree a)) deriving (Show, Eq)

-- |
-- To see the execution of the function in the Haskell debugger I set breakpoints in the Leaf a and Tree a cases.
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

treeT1 = Tree 1 (Seq.fromList [Tree 2 $ Seq.fromList [Leaf 3, Leaf 4], Tree 5 $ Seq.fromList [Leaf 6, Leaf 7, Leaf 8, Leaf 99]])

treeT2 = bfsStopAt (> 8) treeT1

bfsStopAtPath :: forall a. Show a => Int -> ((Seq a) -> Bool) -> Tree a -> Maybe (Seq a)
bfsStopAtPath max pred tree = go 0 pred $ Seq.singleton (Seq.Empty, tree) where
  go :: Int -> (Seq a -> Bool) -> Seq (Seq a, Tree a) -> Maybe (Seq a)
  -- No more nodes to process, stop
  go _ pred (Seq.viewl -> Seq.EmptyL) = Nothing
  go n pred (Seq.viewl -> (zs, Leaf a) Seq.:< xs)
    | n > max = Nothing
    -- found element, stop
    | pred (zs Seq.|> a) = Just $ zs Seq.|> a
    -- go on to next elements in list to process
    | otherwise = go (n + 1) pred xs
  go n pred (Seq.viewl -> (zs, Tree a ys) Seq.:< xs)
    | n > max = Nothing
    -- found element, stop
    | pred (zs Seq.|> a) = Just $ zs Seq.|> a
    -- add all branches of this tree to the list of nodes to process
    | otherwise = go (n + 1) pred $ xs Seq.>< (fmap (\b -> (zs Seq.|> a, b)) ys)

t3 = bfsStopAtPath 4 (\xs -> length xs == 3) treeT1


-- >>> t3

-- I love lazy data structures !!
part1BuildList :: [[a]] -> Tree [a]
part1BuildList xs = Tree [] $ go $ Seq.fromList xs where
  go xs = fmap f xs where
    f x = Tree x $ go xs

applyButtons :: Seq Bool -> Seq [Int] -> Seq Bool
applyButtons xs ys = foldr f xs (concat $ toList ys) where
  f = Seq.adjust' not

-- e agora era para ter todos ligados ?

processMachineV1 (Machine pattern buttons joltages) = case a of
      Just ys -> trace ("ys = " <> show ys) $ Seq.length ys - 1
      Nothing -> error "part1 cannot find solution"
    where 
      a = bfsStopAtPath 100000 pred $ part1BuildList buttons
      startPattern = Seq.replicate (length pattern) False
      seqPattern = Seq.fromList pattern
      pred xs = applyButtons startPattern xs == seqPattern


part1 :: ParsedType -> Int
part1 xs = trace ("smallestSets = " <> show smallestSets) (sum smallestSets) where
  smallestSets = fmap processMachineV1 xs

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
