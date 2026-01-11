{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{- HLINT ignore "Unused LANGUAGE pragma" -}

{--
part1
time: 11m
attempts: 1
used chatgpt: no
notes: very easy, was very similar to day 8. This means part 2 will be hard!

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

type V2 = (Int,Int)
type ParsedType = [V2]

pNumber :: forall a. Read a => Parser a
pNumber = read <$> some digitChar

pNumberLine :: Parser V2
pNumberLine = do
    x <- pNumber
    char ','
    y <- pNumber
    eol
    return (x,y)

parser :: Parser ParsedType
parser = some pNumberLine

area :: Num a => (a, a) -> (a, a) -> a
area (x1,y1) (x2,y2) = (abs dx + 1) * (abs dy + 1) where
  dx = x1 - x2 
  dy = y1 - y2

part1 :: ParsedType -> Int
part1 xs =  head $ sortOn Down $ fmap (uncurry area) ((\[a,b] -> (a,b)) <$> choose 2 xs)

part2 = undefined


-- Tests

genTestString = do
    s <- readFile "test_input"
    putStrLn $  "tString = " <> show s

tString = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3\n"

tParsed = case parse parser "input" tString of
    Right x -> x
    Left _ -> error "not parsed"

-- >>> OnePerLine tParsed
-- (7,1)
-- (11,1)
-- (11,7)
-- (9,7)
-- (9,5)
-- (2,5)
-- (2,3)
-- (7,3)

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

t1 = sortOn (\(a,b,c) -> Down a) $ fmap (\(a,b) -> (uncurry area (a,b), a, b)) ((\[a,b] -> (a,b)) <$> choose 2 tParsed)

-- >>> OnePerLine t1
-- (15,(11,1),(2,5))
-- (15,(11,7),(2,3))
-- (13,(11,1),(2,3))
-- (13,(11,7),(2,5))
-- (13,(9,7),(2,3))
-- (12,(7,1),(11,7))
-- (11,(7,1),(2,5))
-- (11,(9,7),(2,5))
-- (11,(9,5),(2,3))
-- (10,(7,1),(9,7))
-- (10,(11,1),(9,7))
-- (10,(11,7),(7,3))
-- (9,(7,1),(2,3))
-- (9,(9,5),(2,5))
-- (9,(2,5),(7,3))
-- (8,(7,1),(9,5))
-- (8,(11,1),(11,7))
-- (8,(11,1),(9,5))
-- (8,(11,1),(7,3))
-- (8,(9,7),(7,3))
-- (7,(2,3),(7,3))
-- (6,(7,1),(11,1))
-- (6,(11,7),(9,5))
-- (6,(9,5),(7,3))
-- (4,(7,1),(7,3))
-- (4,(11,7),(9,7))
-- (4,(9,7),(9,5))
-- (4,(2,5),(2,3))
