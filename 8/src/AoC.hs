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
time: 2h30
attempts: 2  
used chatgpt: yes, to generate quickcheck tests for part1 
notes: Fairly easy. My first try the example was not giving the right answer, that was because I was not joining groups which had elements from a pair. Looking at the generated groups I was able to determine this manually. Then I didn't get the final answer correct because I overlooked that I should multiply just the 3 largest group sizes. I let ChatGPT generate quickcheck tests for my code, but it passed all the tests. Finally re-reading the challenge I saw the error. Used only normal Haskell code, exec time is ~150ms, so good enough, no need to optimize more.

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
    , part1Test
    , part1Groups
    , getPairsOrderedByDistance
    , part1UniqueLengths
    , part2
    , Homework(..)
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

type Parser = Parsec Void Text

data Operation = Add | Multiply deriving (Show, Eq, Read)

data Homework = Homework [[Natural]] [(Operation,Int)] deriving (Show, Eq, Read)

type V3 = (Int,Int,Int)
type ParsedType = [V3]

pNatural :: Parser Natural
pNatural = read <$> some digitChar

pInt :: Parser Int
pInt = read <$> some digitChar

pFloat :: Parser Float
pFloat = read <$> some digitChar

pNumber :: Read a => Parser a
pNumber = read <$> some digitChar

pNumberLine :: Parser V3
pNumberLine = do
    x <- pNumber
    char ','
    y <- pNumber
    char ','
    z <- pNumber
    eol
    return (x,y,z)

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

-- >>> head $ sortOn snd $ ((\(x:y:[]) -> ((x,y),distanceInt x y) ) <$> choose 2 tParsed)
-- (((162,817,812),(425,690,689)),100427)

-- the two junction boxes which are closest together are 162,817,812 and 425,690,689.

distanceInt :: Num a => (a, a, a) -> (a, a, a) -> a
distanceInt (x1,y1,z1) (x2,y2,z2) = dx*dx + dy*dy + dz*dz where
  dx = x1 - x2
  dy = y1 - y2
  dz = z1 - z2

-- |
-- sortOn: Sort a list by comparing the results of a key function applied to each element. sortOn f is equivalent to sortBy (comparing f), but has the performance advantage of only evaluating f once for each element in the input list. This is called the decorate-sort-undecorate paradigm, or Schwartzian transform.
getPairsOrderedByDistance :: [V3] -> [(V3, V3)]
getPairsOrderedByDistance xs = sortOn (uncurry distanceInt) ((\[a,b] -> (a,b)) <$> choose 2 xs)

-- could get rid of coordinates after calculating distances to get [Int] if needed

connectedComponents :: [(V3, V3)] -> [Set V3]
connectedComponents xs = go xs []
    where
        go [] ys = ys
        go (pair@(a,b):xs) groups = go xs newGroups where
            withElem = fmap (\g -> (g, a `elem` g, b `elem` g)) groups
            (with, without) = partition (\(_,b,c) -> b || c) withElem
            fst3 (x,_,_) = x
            newGroups :: [Set V3]
            newGroups = case with of
              -- a and b are not in any group, create a new group with the two elements 
              [] ->  Set.insert b (Set.singleton a) : groups
              -- single group: if one the two elements is missing from the group, add it
              -- elements in with have second element true or third element true or both
              [(g,aElem,bElem)] -> (if not aElem then Set.insert a g else if not bElem then Set.insert b g else g) : (fst3 <$> without)
              -- very slightly slower (10ms)
              --[(g,aElem,bElem)] -> (Set.insert a $ Set.insert b g) : (fst3 <$> without)
              -- if the elements are in different groups then unite the groups
              zs -> Set.unions (fst3 <$> with) : (fst3 <$> without)

part1Groups :: Int -> [V3] -> [Set V3]
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

[3,2,2]

The next two junction boxes are 431,825,988 and 425,690,689. Because these two junction boxes were already in the same circuit, nothing happens!
--}

test1 = take 10 $ getPairsOrderedByDistance $ tParsed

-- >>> OnePerLine $ test1
-- (V3 162.0 817.0 812.0,V3 425.0 690.0 689.0)
-- (V3 162.0 817.0 812.0,V3 431.0 825.0 988.0)
-- (V3 906.0 360.0 560.0,V3 805.0 96.0 715.0)
-- (V3 431.0 825.0 988.0,V3 425.0 690.0 689.0)
-- (V3 862.0 61.0 35.0,V3 984.0 92.0 344.0)
-- (V3 52.0 470.0 668.0,V3 117.0 168.0 530.0)
-- (V3 819.0 987.0 18.0,V3 941.0 993.0 340.0)
-- (V3 906.0 360.0 560.0,V3 739.0 650.0 466.0)
-- (V3 346.0 949.0 466.0,V3 425.0 690.0 689.0)
-- (V3 906.0 360.0 560.0,V3 984.0 92.0 344.0)

-- >>> length tParsed
-- 20

-- >>> length $ getPairsOrderedByDistance tParsed
-- 190

-- >>>  length $ nub $ concatMap (\(a,b) -> [a,b] )$ take 10 $ getPairsOrderedByDistance $ tParsed
-- 13

test2 = part1Groups 10 tParsed
-- >>> OnePerLine test2
-- fromList [(739,650,466),(805,96,715),(862,61,35),(906,360,560),(984,92,344)]
-- fromList [(162,817,812),(346,949,466),(425,690,689),(431,825,988)]
-- fromList [(819,987,18),(941,993,340)]
-- fromList [(52,470,668),(117,168,530)]

-- 984.0 92.0 344.0 está em dois grupos

-- After making the ten shortest connections, there are 11 circuits: one circuit which contains 5 junction boxes, one circuit which contains 4 junction boxes, two circuits which contain 2 junction boxes each, and seven circuits which each contain a single junction box.
-- 5, 4 , 2, 2

-- >>> fmap length $ part1Groups 10 tParsed
-- [5,4,2,2]

-- >>> part1UniqueLengths 10 tParsed
-- [5,4,2]

-- >>> part1' 10 tParsed
-- 40

part1UniqueLengths :: Int -> [V3] -> [Int]
part1UniqueLengths n xs = nub $ length <$> part1Groups n xs

part1' :: Int -> ParsedType -> Int
part1' n xs = product $ take 3 $ sortOn Down $ part1UniqueLengths n xs

part1Test :: ParsedType -> Int
part1Test = part1' 10

part1 :: ParsedType -> Int
part1 = part1' 1000

-- PART 2

part2 :: Text -> ParsedType -> Int
part2 = undefined

mainTest = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Right parsed -> do
            print $ part1UniqueLengths 1000 parsed
            putStrLn $ "part1: " <> show (part1 parsed) <> "\n"
            --putStrLn $ "part2: " <> show (part2 input parsed) <> "\n"
        Left e -> putStrLn (errorBundlePretty e)
