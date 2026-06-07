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
v4 uses Z3 similarly to part 2, it's 10x faster than my hand made solution

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
v4
  part1v4 SBV without parsing: OK
    344  ms ±  23 ms
  part1 SBV with parsing:      OK
    341  ms ± 5.3 ms

part2
time:
attempts: 1
used chatgpt: For v2 yes, to research a bit linear systems, linear programming and linear programming libraries for Haskell.
notes: The first approach was just to re-use the code from part 1. That attempt is still not finished, it is blowing up.

Looking better at it seemed to me it is solving a linear system but where the solution must have integer coeficients. each button is a column of a matrix, where if the button increments counter i then it has 1 in the matrix.

(3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
b0  b1    b2  b3    b4    b5    
b0 b2 ... b5 is the number of times to press each button

000011 b0 = 3
010001 b1   5
001110 b2   4
110100 b3   7
       b4
       b5

I'm trying to find  x = [b0 b1 b2 b3 b4 b5]^t which solves Ax = b and minimizes Sum in i of bi. 

Searching the internet a bit, it seemed this was a linear programming problem. Asked ChatGPT what are good packages for linear programming in Haskell, but
she suggested it might be easier to just give the problem to SBV, which I already new and I thought it is very cool. So I thought this was a good opportunity to
learn a bit more of SBV and SMT solvers. 

ChatGPT gave me this code to understand how the system works, and I converted it to the full matrix system I have.

import Data.SBV

main :: IO ()
main = do
  res <- optimize Lexicographic $ do
    x1 <- sInteger "x1"
    x2 <- sInteger "x2"
    x3 <- sInteger "x3"
    x4 <- sInteger "x4"
    x5 <- sInteger "x5"
    x6 <- sInteger "x6"

    let xs = [x1, x2, x3, x4, x5, x6]

    -- Natural numbers, allowing 0
    mapM_ (\x -> constrain $ x .>= 0) xs

    constrain $ x1 + x2 + x4      .== 7
    constrain $      x2      + x6 .== 5
    constrain $ x3 + x4 + x5      .== 4
    constrain $           x5 + x6 .== 3

    minimize "sum" (sum xs)

  print res

  It's fast !
  part2 SBV without parsing: OK
    333  ms ±  14 ms
  part2 SBV with parsing:    OK
    329  ms ±  32 ms
--}

module AoC
    ( Parser
    , parser
    , part1
    , part1v4
    , part2
    , part2v2
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
import Data.SBV hiding (some)

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
      res = bfsFoldStop startPattern 100000000 f $ part1BuildList Seq.empty buttons'
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

-- Using SBV 

part1v4 :: ParsedType ->  IO Integer
part1v4 machines = do
  solutions <- traverse part1ProcessMachinev4 machines
  --print solutions
  return $ sum solutions

bToI :: Num a => Bool -> a
bToI True = 1
bToI False = 0

-- |
-- We just state the problem and let Z3 figure it out.
-- This stuff is mind blowing !
part1ProcessMachinev4 :: Machine -> IO Integer
part1ProcessMachinev4 (Machine lights buttons _) = do  
  let 
    -- list of columns
    butCols = fmap (buttonToColumn (length lights)) buttons
    -- list of lines
    lines = transpose butCols
  res <- optLexicographic $ do
    -- create strings for buttons b1 ... bn and create SBV variables
    bs <- traverse (\i -> sInteger $ "b" <> show i) [1..(length buttons)]
    -- number of button presses is non-negative
    forM_ bs $ \b -> constrain $ b .>= 0
    -- Apply matrix multiplication
    let 
      toLiterals = fmap (literal . toInteger)
      f line light = constrain $  (sum (zipWith (*) bs (toLiterals line))) `sMod` 2  .== light 
    sequence_ $ zipWith f lines (toLiterals (bToI <$> lights))
    minimize "sum" (sum bs)
  case getModelValue "sum" res :: Maybe Integer of
    Just a -> return a
    Nothing -> error "no solution for machine"

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
      a = bfsStopAtPath 10000 pred $ part2BuildList buttons'
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

buttonToColumn :: Int -> [Int] -> [Int]
buttonToColumn n xs = [ if i `elem` xs then 1 else 0 | i <- [0..(n-1)]] 

-- Using SBV 

part2v2 :: ParsedType ->  IO Integer
part2v2 machines = do
  solutions <- traverse part2ProcessMachinev2 machines
  --print solutions
  return $ sum solutions

-- |
-- We just state the problem and let Z3 figure it out.
-- This stuff is mind blowing !
part2ProcessMachinev2 :: Machine -> IO Integer
part2ProcessMachinev2 (Machine _ buttons joltages) = do  
  let 
    -- list of columns
    butCols = fmap (buttonToColumn (length joltages)) buttons
    -- list of lines
    lines = transpose butCols
  res <- optLexicographic $ do
    -- create strings for buttons b1 ... bn and create SBV variables
    bs <- traverse (\i -> sInteger $ "b" <> show i) [1..(length buttons)]
    -- number of button presses is non-negative
    forM_ bs $ \b -> constrain $ b .>= 0
    -- Apply matrix multiplication
    let 
      toLiterals = fmap (literal . toInteger)
      f line joltage = constrain $ sum (zipWith (*) bs (toLiterals line))  .== joltage 
    sequence_ $ zipWith f lines (toLiterals joltages)
    minimize "sum" (sum bs)
  case getModelValue "sum" res :: Maybe Integer of
    Just a -> return a
    Nothing -> error "no solution for machine"
























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
