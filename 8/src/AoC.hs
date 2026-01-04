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
    , part1Test
    , part2
    , Homework(..)
    ) where

import Data.List (tails, subsequences, inits, nub, sort, (\\), foldl', transpose, sortOn)
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
import Linear.V3
import Linear.Metric
import Math.Combinat.Sets (combine, choose)

type Parser = Parsec Void Text

data Operation = Add | Multiply deriving (Show, Eq, Read)

data Homework = Homework [[Natural]] [(Operation,Int)] deriving (Show, Eq, Read)

type ParsedType = [V3 Float]

pNatural :: Parser Natural
pNatural = read <$> some digitChar

pInt :: Parser Int
pInt = read <$> some digitChar

pFloat :: Parser Float
pFloat = read <$> some digitChar

pNumber :: Read a => Parser a
pNumber = read <$> some digitChar

pNumberLine :: Parser (V3 Float)
pNumberLine = do
    x <- pNumber
    char ','
    y <- pNumber
    char ','
    z <- pNumber
    eol
    return $ V3 x y z

parser :: Parser ParsedType
parser = some pNumberLine

genTestString = do
    s <- readFile "test_input"
    putStrLn $  "tString = " <> show s

tString = "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689\n"

-- >>> choose 2 [1..4]
-- [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]

-- >>> combine 2 [1..4]
-- [[1,1],[1,2],[1,3],[1,4],[2,2],[2,3],[2,4],[3,3],[3,4],[4,4]]

tParsed = case parse parser "input" tString of
    Right x -> x
    Left _ -> error "not parsed"

-- >>> tParsed

-- >>> choose 2 tParsed

t2 = take 10 $ sortOn snd $ fmap (\(x:y:[]) -> ((x,y),distance x y) )$ choose 2 tParsed
-- >>> t2
-- [((V3 162.0 817.0 812.0,V3 425.0 690.0 689.0),316.9022),((V3 162.0 817.0 812.0,V3 431.0 825.0 988.0),321.56027),((V3 906.0 360.0 560.0,V3 805.0 96.0 715.0),322.36935),((V3 431.0 825.0 988.0,V3 425.0 690.0 689.0),328.11887),((V3 862.0 61.0 35.0,V3 984.0 92.0 344.0),333.65552),((V3 52.0 470.0 668.0,V3 117.0 168.0 530.0),338.3386),((V3 819.0 987.0 18.0,V3 941.0 993.0 340.0),344.3893),((V3 906.0 360.0 560.0,V3 739.0 650.0 466.0),347.5989),((V3 346.0 949.0 466.0,V3 425.0 690.0 689.0),350.78625),((V3 906.0 360.0 560.0,V3 984.0 92.0 344.0),352.93625)]

-- the two junction boxes which are closest together are 162,817,812 and 425,690,689.

-- >>> head $ getPairsOrderedByDistance $ tParsed
-- (V3 162.0 817.0 812.0,V3 425.0 690.0 689.0)



-- |
-- sortOn: Sort a list by comparing the results of a key function applied to each element. sortOn f is equivalent to sortBy (comparing f), but has the performance advantage of only evaluating f once for each element in the input list. This is called the decorate-sort-undecorate paradigm, or Schwartzian transform.
getPairsOrderedByDistance :: [V3 Float] -> [(V3 Float, V3 Float)]
getPairsOrderedByDistance xs = sortOn (uncurry distance) ((\[a,b] -> (a,b)) <$> choose 2 xs)

-- could get rid of coordinates after calculating distances to get [Int] if needed

connectedComponents :: [(V3 Float, V3 Float)] -> [Set (V3 Float)]
connectedComponents xs = go xs []
    where
        go [] ys = ys
        go (pair:xs) groups = go xs newGroups where
            newGroups = updateGroups pair groups
            updateGroups (a,b) [] = [Set.insert b $ Set.singleton a]
            updateGroups (a,b) (group:groups)
              | a `elem` group = Set.insert b group : groups
              | b `elem` group = Set.insert a group : groups
              | otherwise = group : updateGroups pair groups

part1Groups :: Int -> [V3 Float] -> [Set (V3 Float)]
part1Groups n xs = groups where
    pairs = take n $ getPairsOrderedByDistance xs
    groups = connectedComponents pairs

-- by ChatGPT
newtype OnePerLine a = OnePerLine [a]

-- by ChatGPT
instance Show a => Show (OnePerLine a) where
  show :: Show a => OnePerLine a -> String
  show (OnePerLine xs) = unlines (map show xs)

{--
To save on string lights, the Elves would like to focus on connecting pairs of junction boxes that are as close together as possible according to straight-line distance. In this example, the two junction boxes which are closest together are 162,817,812 and 425,690,689.

By connecting these two junction boxes together, because electricity can flow between them, they become part of the same circuit. After connecting them, there is a single circuit which contains two junction boxes, and the remaining 18 junction boxes remain in their own individual circuits.

Now, the two junction boxes which are closest together but aren't already directly connected are 162,817,812 and 431,825,988. After connecting them, since 162,817,812 is already connected to another junction box, there is now a single circuit which contains three junction boxes and an additional 17 circuits which contain one junction box each.

The next two junction boxes to connect are 906,360,560 and 805,96,715. After connecting them, there is a circuit containing 3 junction boxes, a circuit containing 2 junction boxes, and 15 circuits which contain one junction box each.

The next two junction boxes are 431,825,988 and 425,690,689. Because these two junction boxes were already in the same circuit, nothing happens!
--}

-- >>> OnePerLine $ part1Groups 10 tParsed
-- fromList [V3 162.0 817.0 812.0,V3 346.0 949.0 466.0,V3 425.0 690.0 689.0,V3 431.0 825.0 988.0]
-- fromList [V3 739.0 650.0 466.0,V3 805.0 96.0 715.0,V3 906.0 360.0 560.0,V3 984.0 92.0 344.0]
-- fromList [V3 862.0 61.0 35.0,V3 984.0 92.0 344.0]
-- fromList [V3 52.0 470.0 668.0,V3 117.0 168.0 530.0]
-- fromList [V3 819.0 987.0 18.0,V3 941.0 993.0 340.0]

-- After making the ten shortest connections, there are 11 circuits: one circuit which contains 5 junction boxes, one circuit which contains 4 junction boxes, two circuits which contain 2 junction boxes each, and seven circuits which each contain a single junction box.
-- >>> fmap length $ part1Groups 10 tParsed
-- [4,4,2,2,2]

part1' :: Int -> ParsedType -> Int
part1' n xs = product (length <$> part1Groups n xs) 

part1Test :: ParsedType -> Int
part1Test = part1' 10

part1 :: ParsedType -> Int
part1 = part1' 1000

-- PART 2

part2 :: Text -> ParsedType -> Int
part2 = undefined

