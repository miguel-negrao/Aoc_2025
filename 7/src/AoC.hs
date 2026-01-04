{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

{--
notes:
part1
time: 1h15m
attempts: 1
used chatgpt: yes, to add a memoization test and then change slightly the memoization function: "Updated loopTableStore to memoize both the input and output stores by adding a memoizeTableStore helper and composing it before and after extend. This change ensures each state’s lookup function is cached (so repeated peeks don’t re-run the evolution rule) and also keeps repeated lookups inside the rule cheap by memoizing the prior state." 
Also used it to try to understand why my solution is so slow.

notes:

After reading my initial idea is to use comonads with tables. The evolution rules are if this cell is | put a | below, except if below is ^ put on |^|. Stop when state doesn't change on the whole board. At the end count the number ^ with | above. The question is going to be if my memoization scheme is really working and is good enough for the size of the table.
    
Done but takes 35s seems a bit too long, it's a pity as the code is really elegant. Optimizing a bit goes to 12s. It appears there are solutions with ~0.0005 s.
    
Looking at this as graph problem, the splitters where the ray is split are the vertices, together with the start position and then end positions of the rays when they leave the chamber. The question is how many vertices are there if we exclude the start and end positions.

part2
time: 
attempts: 
used chatgpt: 
notes: initial thoughts: if part1 takes 30s part 2 cannot be done with comonads... perhaps create a graph and go away from the table, then check all possible ways to walk the graph from a start point to an end point. 

Benchmark bench: RUNNING...
All
  part1: OK
    35.431 s ± 182 ms

All 1 tests passed (106.30s)

--}

module AoC
    (

      part1
    , part2
    , TableStore
    , loopTableStore
    , slowLoop
    , tableToTableStore
    ) where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace (trace)
import GHC.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, char, newline)
import Control.Error

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Comonad
import Control.Comonad.Store
import qualified Data.MemoCombinators as Memo
import Data.MemoCombinators (Memo)
import Control.Lens
import Data.Maybe (catMaybes, fromMaybe)
import Control.Comonad.Env (EnvT(..), ask, runEnvT)
import Control.Monad (guard)
import GHC.IO.Encoding (BufferCodec(encode))
import Data.Bits (Bits(xor))

-- COMONAD  STUFF

-- reading material 
-- - https://blog.ielliott.io/comonad-transformers-in-the-wild <--- quite simple and similar to this case
-- - https://chrispenner.ca/posts/conways-game-of-life
-- - http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html
-- https://idontgetoutmuch.wordpress.com/2013/02/23/comonads-life-and-klein-bottles/
-- https://mis.pm/hascell-conway
-- https://www.hacklewayne.com/also-on-comonad-and-conway-s-game-of-life
-- https://bartoszmilewski.com/2025/01/04/legalizing-comonad-composition/ <--- this also mentions 2D finite grids
-- https://work.njae.me.uk/2020/12/06/advent-of-code-2019-day-24/
-- https://www.schoolofhaskell.com/user/edwardk/cellular-automata/part-1
-- https://jaspervdj.be/posts/2014-11-27-comonads-image-processing.html <--- images, doesn't use Store, just comonad directly

-- - https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/cellular-automata
-- it explains how to go from 

-- | This evaluator does not memoize. Not usable for many state changes.
slowLoop f = iterate (extend f)

-- to 
-- data StoreT s w a = StoreT (w (s -> a)) s

-- |
-- Ed Kmett: tab takes a way to memoize a function from the context of our Store and a Store and yields a new Store that memoizes its results. Memo comes from data-memocombinators.
--
-- MNegrao: I've ajusted what was in the blog post to StoreT
tab :: Memo s -> Store s a -> Store s a
tab opt (StoreT wf s) = StoreT (fmap opt wf) s

-- | By Ed Kmett. This should memoize.
loop :: Integral s => (Store (s,s) a -> a) -> Store (s,s) a -> [Store (s,s) a]
loop f = iterate (extend f . tab (Memo.pair Memo.integral Memo.integral))

-- The comonad functions
-- extract :: w a -> a                      get the current focused value
-- extend :: (w a -> b) -> w a -> w b       calculate new state based on neighbors. The w a given to f is focused on the current element under anaysis

-- The Store functions
-- pos :: w a -> s          get current focused position
-- peek :: s -> w a -> a    given a position and the store check what is there. This is how you check the neighbours.

-- to create a store I need to use this function
-- store :: (s -> a) -> s -> Store s a
-- store f initialFocus. 
-- f given a coordinate I give you the value there
-- initialFocus is clear

-- f within the board I can return Just x
-- f outside the board I can return Nothing
-- initial table is a Map of positions to values.


type Width = Int
type Height = Int

-- x is column increases from left to right
-- y is row increses up to down
type Position = (Int,Int)
type Table a = Map Position a

-- | The EnvT comonad is used to keep the original width and height
--   The element type is Bool, this way elements outside the original grid are considered to not have a paper roll because False is returned by peek. 
type TableStore a = EnvT (Width,Height) (Store Position) a

tableDimensions :: [[a]] -> (Int,Int)
tableDimensions xs = (w,h) where
    w = case xs of
        [] -> error "A table list cannot be empty has to have at least an empty list inside it"
        (x:_) -> length x
    h = length xs

listToTable :: [[a]] -> Table a
listToTable xs = Map.fromList $ concat $ imap (\j line -> imap (\i e -> ((i,j), e)) line) xs

listToStore :: [[TachionManifoldEntity]] -> TableStore TachionManifoldEntity
listToStore xs = tableToTableStore w h $ listToTable xs where
    (w,h) = tableDimensions xs

up :: Position
up = (0,-1)

down :: Position
down = (0,1)

left :: Position
left = (-1,0)

right :: Position
right = (1,0)

sumPair :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumPair (a,b) (c,d) = (a+c,b+d)

storeToCoordList :: TableStore a -> [((Int,Int), a)]
storeToCoordList s = do
    let (w,h) = ask s
    i <- [0..(w-1)]
    j <- [0..(h-1)]
    return ((i,j), peek (i,j) s)

storeToTable :: TableStore a -> Table a
storeToTable s = Map.fromList $ storeToCoordList s

storeToList :: TableStore a -> [[a]]
storeToList s = do
    let (w, h) = ask s
    j <- [0..(h-1)]
    return $ do
        i <- [0..(w-1)]
        return $ peek (i,j) s

memoizeTableStore :: TableStore a -> TableStore a
memoizeTableStore s =
    let (e,s') = runEnvT s
    in EnvT e (tab (Memo.pair Memo.integral Memo.integral) s')

loopTableStore :: (TableStore a -> a) -> TableStore a -> [TableStore a]
loopTableStore f = iterate (memoizeTableStore . extend f )
-- this appears to be faster than memoizeTableStore . extend f . memoizeTableStore  or extend f . memoizeTableStore 

findStableState :: Eq a => Int -> (TableStore a -> a) -> TableStore a -> Maybe (TableStore a)
findStableState maxInterations f s
    | maxInterations <= 1 = Nothing
    | otherwise =  let
            xs = take maxInterations $ loopTableStore f s
            ys = zip xs (drop 1 xs)
            zs = filter (\(a,b) -> storeToList a == storeToList b) ys
        in fst <$> headMay zs

findStableStateError :: Eq a => Int -> (TableStore a -> a) -> TableStore a -> TableStore a
findStableStateError maxInterations f s =
    let
        x = findStableState maxInterations f s
    in case x of
        Just x -> x
        Nothing -> error $ "findStableStateError would not stop after " <> show maxInterations <> " interations"

countTrue :: TableStore Bool -> Int
countTrue = length . concatMap (filter id) . storeToList

genTestString = do
    s <- readFile "test_input"
    putStrLn $  "tString = " <> show s

tString :: String
tString = ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............\n"

tStore = listToStore $ stringToList tString

-- >>> storeToString $ head $ drop 4 $  loopTableStore rule tStore
-- ".......S.......\n.......|.......\n......|^|......\n......|.|......\n.....|^|^|.....\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............\n"

-- These functions are specific to the type of Table
data TachionManifoldEntity = EmptySpace | Ray | Splitter | StartPosition deriving (Eq, Show, Read)

stringToList :: String -> [[TachionManifoldEntity]]
stringToList = fmap (fmap f) . lines where
    f '|' = Ray
    f '^' = Splitter
    f '.' = EmptySpace
    f 'S' = StartPosition
    f c = error $ "string can only have | ^ and spaces. found : " <> show c

listToString :: [[TachionManifoldEntity]] -> String
listToString xs = unlines $ fmap (fmap f) xs where
    f EmptySpace = '.'
    f Ray = '|'
    f Splitter = '^'
    f StartPosition = 'S'

storeToString :: TableStore TachionManifoldEntity -> String
storeToString = listToString . storeToList

tableToTableStore :: Width -> Height -> Table TachionManifoldEntity -> TableStore TachionManifoldEntity
tableToTableStore w h xs =
    let
        f (i,j) = fromMaybe EmptySpace $ Map.lookup (i,j) xs
    in EnvT (w,h) $ store f (0,0)


-- >>> storeToString (listToStore $ stringToList tString) == tString
-- True

rule :: TableStore TachionManifoldEntity -> TachionManifoldEntity
rule s =
    let
        current = extract s
        peek' p = peeks (sumPair p) s
    in case current of
        EmptySpace -> if
            (peek' up `elem` [Ray, StartPosition]) ||
            (peek' left == Splitter && peek' (-1,-1) == Ray) ||
            (peek' right == Splitter && peek' (1,-1) == Ray) then Ray else EmptySpace
        _ -> current

countSplits :: TableStore TachionManifoldEntity -> Bool
countSplits s =
    let
        current = extract s
        peek' p = peeks (sumPair p) s
    in case current of
        Splitter -> peek' up == Ray
        _ -> False

countTrueList :: [Bool] -> Int
countTrueList = length . filter id

-- 22s
part1V1 :: String -> Int
part1V1 xs =
            let
                initial = listToStore $ stringToList xs
                (w,h) = ask initial
                final = head $ drop (h - 1) $ loopTableStore rule initial
            in countTrue $ extend countSplits final

type TableStore2 a = EnvT (Width,Height) (Store Int) a
type Table2 a = Map Int a
-- |
-- Put lines next to each other and then index as a list
encodePos :: Width -> Height -> Position -> Int
encodePos w h (x,y) = (y * w) + x

decodePos :: Width -> Height -> Int -> Position
decodePos w h z = let (a,b) = divMod z w in (b, a)

listToTable2 :: Width -> Height ->  [[a]] -> Table2 a
listToTable2 w h xs = Map.fromList $ concat $ imap (\j line -> imap (\i e -> (encodePos w h (i,j), e)) line) xs

tableToTableStore2 :: Width -> Height -> Table2 TachionManifoldEntity -> TableStore2 TachionManifoldEntity
tableToTableStore2 w h xs =
    let
        f z = fromMaybe EmptySpace $ Map.lookup z xs
    in EnvT (w,h) $ store f 0

listToStore2 :: [[TachionManifoldEntity]] -> TableStore2 TachionManifoldEntity
listToStore2 xs = tableToTableStore2 w h $ listToTable2 w h xs where
      (w,h) = tableDimensions xs

peekPos :: Position -> TableStore2 a -> a
peekPos pair s =
    let
        (w,h) = ask s
    in peek (encodePos w h pair) s

peeksPos :: (Position -> Position) -> TableStore2 a -> a
peeksPos f s =
    let
        (w,h) = ask s
    in peeks (encodePos w h . f . decodePos w h) s

storeToCoordList2 :: TableStore2 a -> [(Int, a)]
storeToCoordList2 s = do
    let (w,h) = ask s
    i <- [0..(w-1)]
    j <- [0..(h-1)]
    return (encodePos w h (i,j), peek (encodePos w h (i,j)) s)

storeToTable2 :: TableStore2 a -> Table2 a
storeToTable2 s = Map.fromList $ storeToCoordList2 s

storeToList2 :: TableStore2 a -> [[a]]
storeToList2 s = do
    let (w, h) = ask s
    j <- [0..(h-1)]
    return $ do
        i <- [0..(w-1)]
        return $ peekPos (i,j) s

memoizeTableStore2 :: TableStore2 a -> TableStore2 a
memoizeTableStore2 s =
    let (e,s') = runEnvT s
    in EnvT e (tab Memo.integral s')

loopTableStore2 :: (TableStore2 a -> a) -> TableStore2 a -> [TableStore2 a]
loopTableStore2 f = iterate (memoizeTableStore2 . extend f )

countTrue2 :: TableStore2 Bool -> Int
countTrue2 s = let (w, h) = ask s in countTrueList $ [ peek i s | i <- [0..((w*h)-1)]]

rule2 :: TableStore2 TachionManifoldEntity -> TachionManifoldEntity
rule2 s =
    let
        current = extract s
        peek' p = peeksPos (sumPair p) s
    in case current of
        EmptySpace -> if
            (peek' up `elem` [Ray, StartPosition]) ||
            (peek' left == Splitter && peek' (-1,-1) == Ray) ||
            (peek' right == Splitter && peek' (1,-1) == Ray) then Ray else EmptySpace
        _ -> current

countSplits2 :: TableStore2 TachionManifoldEntity -> Bool
countSplits2 s =
    let
        current = extract s
        peek' p = peeksPos (sumPair p) s
    in case current of
        Splitter -> peek' up == Ray
        _ -> False

-- |12.3 s
part1V2 :: String -> Int
part1V2 xs =
            let
                initial = listToStore2 $ stringToList xs
                (w,h) = ask initial
                final = head $ drop (h - 1) $ loopTableStore2 rule2 initial
            in countTrue2 $ extend countSplits2 final

downFrom (a,b) = (a,b+1)
downLeftFrom (a,b) = (a-1, b+1)
downRightFrom (a,b) = (a+1,b+1)

part1V3' :: Position -> Table TachionManifoldEntity -> [Position]
part1V3' start t = let d = downFrom start in case Map.lookup d t of
    Nothing -> []
    Just Splitter -> d:part1V3' (downLeftFrom start) t ++ part1V3' (downRightFrom start) t
    Just _ -> part1V3' d t

-- | too long...
part1V3 :: String -> Int
part1V3 xs =
            let
                table = listToTable $ stringToList xs
                start = fromMaybe (error "no S") $ elemIndex 'S' xs
                positions = part1V3' (start,0) table
            in  length $ nub positions

-- |
-- This is based on the classic DFS algorithm for graphs as adjacency lists in Haskell as explained by ChatGPT.
-- After 7 minutes was not done
part1V4' :: Position -> Table TachionManifoldEntity -> [Position]
part1V4' start t =
    let
        go seen [] = []
        go seen (x:xs) = case Map.lookup x t of
            Nothing -> go seen xs
            Just Splitter ->  
                if x `elem` seen then go seen xs else x : go (Set.insert x seen) ([downLeftFrom start, downRightFrom start] ++ xs)
            Just _ -> go seen (downFrom x:xs)
    in go Set.empty [start]


part1V4 :: String -> Int
part1V4 xs =
            let
                table = listToTable $ stringToList xs
                start = fromMaybe (error "no S") $ elemIndex 'S' xs
                positions = part1V4' (start,0) table
            in  length positions


part1 :: String -> Int
part1 = part1V3


part2' :: Position -> Table TachionManifoldEntity -> Int
part2' start t = let d = downFrom start in case Map.lookup d t of
    Nothing -> 1
    Just Splitter -> part2' (downLeftFrom start) t + part2' (downRightFrom start) t
    Just _ -> part2' d t

-- | too long...
part2V1 :: String -> Int
part2V1 xs =
            let
                table = listToTable $ stringToList xs
                start = fromMaybe (error "no S") $ elemIndex 'S' xs
            in  part2' (start,0) table

part2 :: String -> Int
part2 = part2V1
