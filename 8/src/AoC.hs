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
notes: Fairly easy. My first try the example was not giving the right answer, that was because I was not joining 2 different groups which had elements from a pair. Looking at the generated groups I was able to determine this manually. Then I didn't get the final answer correct because I overlooked that I should multiply just the 3 largest group sizes and not all. At this point, I asked ChatGPT to generate quickcheck tests for my code, but it passed all the tests. Finally re-reading the challenge I saw the error. Used only normal Haskell code, exec time is ~150ms, so good enough, no need to optimize more.

part2
time: 1h30
attempts: 2
used chatgpt: no
notes: First attempt was naive, calculating everything over and over, and it took more than a second. So changed to a recursive function that calculates directly and stops when the all nodes are in one group.

Benchmark bench: RUNNING...
All
  part1 without parsing: OK
    154  ms ± 3.1 ms
  part2 without parsing: OK
    243  ms ±  19 ms
  part1 with parsing:    OK
    212  ms ± 8.1 ms
  part2 with parsing:    OK
    304  ms ±  12 ms

All 4 tests passed (9.31s)
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

type Parser = Parsec Void Text

type V3 = (Int,Int,Int)
type ParsedType = [V3]

pNumber :: forall a . Read a => Parser a
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

part1UniqueLengths :: Int -> [V3] -> [Int]
part1UniqueLengths n xs = nub $ length <$> part1Groups n xs

part1' :: Int -> ParsedType -> Int
part1' n xs = product $ take 3 $ sortOn Down $ part1UniqueLengths n xs

part1Test :: ParsedType -> Int
part1Test = part1' 10

part1 :: ParsedType -> Int
part1 = part1' 1000

-- PART 2

part2Vectors :: [V3] -> (V3, V3)
part2Vectors xs = trace ("part2Vectors  = " <> show result) result where
    result = head $ fmap snd $ filter (\ys -> let ys' = fst ys in length ys' == 1 && length (head ys') == l) $ fmap (\n -> (connectedComponents $ take n sorted, last $ take n sorted)) [2..] 
    sorted = getPairsOrderedByDistance xs
    l = length xs

-- >>> part2Vectors tParsed
-- ((216,146,977),(117,168,530))

-- Continuing the above example, the first connection which causes all of the junction boxes to form a single circuit is between the junction boxes at 216,146,977 and 117,168,530. The Elves need to know how far those junction boxes are from the wall so they can pick the right extension cable; multiplying the X coordinates of those two junction boxes (216 and 117) produces 25272.

calcX ((x1,_,_), (x2,_,_)) = x1 * x2 

-- Slow
part2V1 ::ParsedType -> Int
part2V1 xs = calcX $ part2Vectors xs 


connectedComponentsV2 :: Int -> [(V3, V3)] -> (V3, V3)
connectedComponentsV2 n xs = go xs []
    where
        isSingleConnectionWithAllNodes [] = False
        isSingleConnectionWithAllNodes [zs] = length zs == n
        isSingleConnectionWithAllNodes _ = False
        go [] ys = error "Ran out of connections"
        go (pair@(a,b):xs) groups = if isSingleConnectionWithAllNodes newGroups then pair else next where
            next = go xs newGroups 
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

part2V2 xs = calcX $ connectedComponentsV2 n sorted where
    sorted = getPairsOrderedByDistance xs
    n = length xs

part2 = part2V2


















-- Tests

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

test2 = part1Groups 10 tParsed
-- >>> OnePerLine test2
-- fromList [(739,650,466),(805,96,715),(862,61,35),(906,360,560),(984,92,344)]
-- fromList [(162,817,812),(346,949,466),(425,690,689),(431,825,988)]
-- fromList [(819,987,18),(941,993,340)]
-- fromList [(52,470,668),(117,168,530)]

mainTest = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Right parsed -> do
            print $ part1UniqueLengths 1000 parsed
            putStrLn $ "part1: " <> show (part1 parsed) <> "\n"
            --putStrLn $ "part2: " <> show (part2 input parsed) <> "\n"
        Left e -> putStrLn (errorBundlePretty e)

-- >>> OnePerLine $ getPairsOrderedByDistance tParsed
-- ((162,817,812),(425,690,689))
-- ((162,817,812),(431,825,988))
-- ((906,360,560),(805,96,715))
-- ((431,825,988),(425,690,689))
-- ((862,61,35),(984,92,344))
-- ((52,470,668),(117,168,530))
-- ((819,987,18),(941,993,340))
-- ((906,360,560),(739,650,466))
-- ((346,949,466),(425,690,689))
-- ((906,360,560),(984,92,344))
-- ((592,479,940),(425,690,689))
-- ((352,342,300),(542,29,236))
-- ((352,342,300),(117,168,530))
-- ((352,342,300),(466,668,158))
-- ((542,29,236),(862,61,35))
-- ((592,479,940),(431,825,988))
-- ((739,650,466),(425,690,689))
-- ((162,817,812),(52,470,668))
-- ((819,987,18),(970,615,88))
-- ((805,96,715),(984,92,344))
-- ((466,668,158),(739,650,466))
-- ((162,817,812),(346,949,466))
-- ((739,650,466),(941,993,340))
-- ((57,618,57),(466,668,158))
-- ((52,470,668),(425,690,689))
-- ((466,668,158),(346,949,466))
-- ((739,650,466),(970,615,88))
-- ((970,615,88),(941,993,340))
-- ((216,146,977),(117,168,530)) <-------------------------
-- ((542,29,236),(984,92,344))
-- ((57,618,57),(352,342,300))
-- ((52,470,668),(216,146,977))
-- ((352,342,300),(52,470,668))
-- ((592,479,940),(805,96,715))
-- ((739,650,466),(346,949,466))
-- ((466,668,158),(819,987,18))
-- ((592,479,940),(216,146,977))
-- ((906,360,560),(592,479,940))
-- ((466,668,158),(970,615,88))
-- ((352,342,300),(739,650,466))
-- ((592,479,940),(739,650,466))
-- ((352,342,300),(425,690,689))
-- ((466,668,158),(425,690,689))
-- ((542,29,236),(117,168,530))
-- ((906,360,560),(970,615,88))
-- ((431,825,988),(346,949,466))
-- ((542,29,236),(805,96,715))
-- ((162,817,812),(592,479,940))
-- ((739,650,466),(819,987,18))
-- ((970,615,88),(862,61,35))
-- ((970,615,88),(984,92,344))
-- ((906,360,560),(542,29,236))
-- ((52,470,668),(346,949,466))
-- ((906,360,560),(425,690,689))
-- ((57,618,57),(346,949,466))
-- ((466,668,158),(941,993,340))
-- ((592,479,940),(52,470,668))
-- ((906,360,560),(862,61,35))
-- ((346,949,466),(941,993,340))
-- ((431,825,988),(52,470,668))
-- ((739,650,466),(805,96,715))
-- ((906,360,560),(352,342,300))
-- ((739,650,466),(984,92,344))
-- ((117,168,530),(425,690,689))
-- ((57,618,57),(52,470,668))
-- ((352,342,300),(346,949,466))
-- ((431,825,988),(739,650,466))
-- ((352,342,300),(862,61,35))
-- ((216,146,977),(805,96,715))
-- ((466,668,158),(542,29,236))
-- ((216,146,977),(425,690,689))
-- ((819,987,18),(346,949,466))
-- ((57,618,57),(117,168,530))
-- ((352,342,300),(805,96,715))
-- ((906,360,560),(466,668,158))
-- ((906,360,560),(941,993,340))
-- ((352,342,300),(984,92,344))
-- ((805,96,715),(862,61,35))
-- ((466,668,158),(52,470,668))
-- ((542,29,236),(739,650,466))
-- ((941,993,340),(425,690,689))
-- ((162,817,812),(216,146,977))
-- ((162,817,812),(739,650,466))
-- ((592,479,940),(352,342,300))
-- ((592,479,940),(117,168,530))
-- ((805,96,715),(425,690,689))
-- ((352,342,300),(970,615,88))
-- ((162,817,812),(117,168,530))
-- ((592,479,940),(346,949,466))
-- ((431,825,988),(216,146,977))
-- ((466,668,158),(117,168,530))
-- ((117,168,530),(805,96,715))
-- ((352,342,300),(216,146,977))
-- ((162,817,812),(352,342,300))
-- ((57,618,57),(425,690,689))
-- ((466,668,158),(862,61,35))
-- ((162,817,812),(466,668,158))
-- ((739,650,466),(52,470,668))
-- ((739,650,466),(862,61,35))
-- ((542,29,236),(970,615,88))
-- ((57,618,57),(542,29,236))
-- ((162,817,812),(57,618,57))
-- ((542,29,236),(52,470,668))
-- ((739,650,466),(117,168,530))
-- ((906,360,560),(431,825,988))
-- ((57,618,57),(739,650,466))
-- ((466,668,158),(984,92,344))
-- ((346,949,466),(970,615,88))
-- ((542,29,236),(425,690,689))
-- ((592,479,940),(984,92,344))
-- ((906,360,560),(117,168,530))
-- ((592,479,940),(466,668,158))
-- ((970,615,88),(425,690,689))
-- ((117,168,530),(346,949,466))
-- ((542,29,236),(216,146,977))
-- ((906,360,560),(346,949,466))
-- ((805,96,715),(970,615,88))
-- ((819,987,18),(425,690,689))
-- ((906,360,560),(819,987,18))
-- ((906,360,560),(216,146,977))
-- ((592,479,940),(542,29,236))
-- ((431,825,988),(941,993,340))
-- ((52,470,668),(805,96,715))
-- ((352,342,300),(431,825,988))
-- ((352,342,300),(819,987,18))
-- ((466,668,158),(431,825,988))
-- ((57,618,57),(819,987,18))
-- ((431,825,988),(117,168,530))
-- ((431,825,988),(805,96,715))
-- ((592,479,940),(941,993,340))
-- ((466,668,158),(805,96,715))
-- ((906,360,560),(52,470,668))
-- ((352,342,300),(941,993,340))
-- ((739,650,466),(216,146,977))
-- ((984,92,344),(425,690,689))
-- ((117,168,530),(984,92,344))
-- ((117,168,530),(862,61,35))
-- ((941,993,340),(984,92,344))
-- ((162,817,812),(906,360,560))
-- ((57,618,57),(970,615,88))
-- ((819,987,18),(862,61,35))
-- ((162,817,812),(941,993,340))
-- ((592,479,940),(970,615,88))
-- ((216,146,977),(346,949,466))
-- ((819,987,18),(984,92,344))
-- ((542,29,236),(346,949,466))
-- ((162,817,812),(805,96,715))
-- ((57,618,57),(862,61,35))
-- ((805,96,715),(941,993,340))
-- ((941,993,340),(862,61,35))
-- ((216,146,977),(984,92,344))
-- ((805,96,715),(346,949,466))
-- ((57,618,57),(941,993,340))
-- ((466,668,158),(216,146,977))
-- ((862,61,35),(425,690,689))
-- ((57,618,57),(906,360,560))
-- ((542,29,236),(819,987,18))
-- ((57,618,57),(431,825,988))
-- ((592,479,940),(862,61,35))
-- ((57,618,57),(592,479,940))
-- ((162,817,812),(819,987,18))
-- ((57,618,57),(216,146,977))
-- ((162,817,812),(542,29,236))
-- ((542,29,236),(941,993,340))
-- ((52,470,668),(984,92,344))
-- ((431,825,988),(819,987,18))
-- ((117,168,530),(970,615,88))
-- ((431,825,988),(970,615,88))
-- ((346,949,466),(984,92,344))
-- ((592,479,940),(819,987,18))
-- ((52,470,668),(941,993,340))
-- ((52,470,668),(970,615,88))
-- ((542,29,236),(431,825,988))
-- ((162,817,812),(970,615,88))
-- ((57,618,57),(984,92,344))
-- ((52,470,668),(862,61,35))
-- ((346,949,466),(862,61,35))
-- ((431,825,988),(984,92,344))
-- ((57,618,57),(805,96,715))
-- ((52,470,668),(819,987,18))
-- ((819,987,18),(805,96,715))
-- ((216,146,977),(862,61,35))
-- ((117,168,530),(941,993,340))
-- ((162,817,812),(984,92,344))
-- ((819,987,18),(117,168,530))
-- ((216,146,977),(970,615,88))
-- ((216,146,977),(941,993,340))
-- ((162,817,812),(862,61,35))
-- ((431,825,988),(862,61,35))
-- ((216,146,977),(819,987,18))
