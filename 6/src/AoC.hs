{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Unused LANGUAGE pragma" -}

{--
part1
time: 1h10m
attempts: 1  
used chatgpt: yes, to investigate looping with megaparsec
notes: my parsers were looping. Finally had to resort to chatgpt to suggest using sepEndBy. It's hard to write that function by hand.

part2
time: ~1h
attempts: 1
used chatgpt: no
notes: not hard, just had to be careful with transpose

All
  part1: OK
    89.7 μs ± 4.7 μs
  part2: OK
    3.60 ms ± 175 μs

All 2 tests passed (2.70s)
--}

module AoC
    ( Parser
    , parser
    , part1
    , part2
    , Homework(..)
    ) where

import Data.List (tails, subsequences, inits, nub, sort, (\\), foldl', transpose)
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

-- comonad stuff
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

type Parser = Parsec Void Text

data Operation = Add | Multiply deriving (Show, Eq, Read)

data Homework = Homework [[Natural]] [(Operation,Int)] deriving (Show, Eq, Read)

type ParsedType = Homework


pNumber :: forall a. Read a => Parser a
pNumber = read <$> some digitChar

pNumberLine :: Parser [Natural]
pNumberLine = do
    hspace
    numbers <- pNumber @Natural `sepEndBy` hspace1
    newline
    return numbers


pOp :: Parser (Operation,Int)
pOp = do
    pos <- getSourcePos
    op <- char '+' $> Add <|> char '*' $> Multiply
    return (op, unPos $ sourceColumn pos)

pOperationsLine :: Parser [(Operation,Int)]
pOperationsLine = do
    hspace
    ops <- pOp `sepEndBy` hspace1
    newline
    return ops

parser :: Parser ParsedType
parser = do
    a <- some $ try pNumberLine
    Homework a <$> pOperationsLine

tString :: Text
tString = "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  \n"

tParsed = case parse parser "input" tString of
    Right x -> x
    Left _ -> error "not parsed"

-- >>> tParsed
-- Homework [[123,328,51,64],[45,64,387,23],[6,98,215,314]] [(Multiply,1),(Add,5),(Multiply,9),(Add,13)]

doOperation :: (Foldable t, Num a) => t a -> (Operation, b) -> a
doOperation xs (Add,_) = sum xs
doOperation xs (Multiply,_) = product xs

doSums xs ops = fromIntegral $ sum $ zipWith doOperation xs ops

part1 :: ParsedType -> Int
part1 (Homework numbers ops) = doSums (Data.List.transpose numbers) ops

-- PART 2

-- |
-- Splits receives a list of cut points, and cuts the list in segments. The element to the left of the cut point is discarded (it will be whitespace).
splits :: [Int] -> [a] -> [[a]]
splits = splitsGo 0 where
    splitsGo _ [] xs = [xs]
    splitsGo n (y:ys) xs = let (a,b:bs) = splitAt  (y - n - 1) xs in a : splitsGo y ys bs

-- |
-- The position given by Megaparsec start at 1 apparently so I use pred to get list indexes correctly
-- This applies @splits@ to everyline and then transposes to get each problem subarray of text
splitNumbers :: [[a]] -> [(b, Int)] -> [[[a]]]
splitNumbers lines (x:xs) = let
    splitPos = fmap (pred. snd) xs
    in transpose $ fmap (splits splitPos) lines

part2 :: Text -> ParsedType -> Int
part2 input (Homework numbers ops) = let
    problems = splitNumbers (init $ lines (T.unpack input)) ops
    numberLists = fmap (fmap read . transpose) problems
    in  doSums numberLists ops












-- tests
-- I solved it by woking out with the example given

-- >>> splits [5,10] [1..20]
-- [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15,16,17,18,19,20]]

-- >>>  head $ transpose $ [splits [5,10] [1..20], splits [5,10] [1..20], splits [5,10] [1..20]]
-- [[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]


{--
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
--}

--t1 = case tParsed of Homework _ ops -> splitNumbers (init $ lines (T.unpack tString)) ops

-- >>> t1
-- [["123"," 45","  6"],["328","64 ","98 "],[" 51","387","215"],["64 ","23 ","314"]]

--t2 = head t1
-- >>> t2
-- ["123 "," 45 ","  6 "]

{--
123 '
 45 '
  6 '

transposed
1  '
24 '
456'
   '
--}


-- >>> transpose t2
-- ["1  ","24 ","356","   "]
--t3 :: [Int]
--t3 = fmap read $ transpose t2

-- >>> t3
-- Prelude.read: no parse
