{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

{--
notes:
part1
time: 3h 25m
attempts: 1
used chatgpt: yes, to read up a bit on comonads, and find relevant blog posts. Not to solve the exercise.
notes: Took almost all the time to setup the comonad structure. Ended up going for a Map based Store comonad with an EnvT to keep the original width and height.

part2
time: 34m
attempts: 1
used chatgpt: no
notes: with the machinery of the store comonad this is completely basic as long the memoization really works. The heavy lifting is done by loopTableStore which is essentially code by Edward Kmett. After day 3 it feels good to get one on first attempt :-).

All
  part1: OK
    23.6 ms ± 1.9 ms
  part2: OK
    2.471 s ± 225 ms

All 2 tests passed (7.83s)
--}

module Aoc4
    ( Parser
    , parser
    , part1
    , part2
    ) where

import Data.List (tails, subsequences, inits)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace (trace)
import GHC.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, char, newline)
import Control.Error

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

type Parser = Parsec Void Text

pLine :: Parser [Bool]
pLine = do
    xs <- many (char '.' $> False <|> (char '@' $> True))
    newline
    return xs

type ParsedType = [[Bool]]

parser :: Parser ParsedType
parser = many pLine

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


loopTableStore :: (TableStore -> Bool) -> TableStore -> [TableStore]
loopTableStore f = iterate (extend f . (\s -> let (e,s') = runEnvT s in EnvT e (tab (Memo.pair Memo.integral Memo.integral) s')))

findStableState :: Int -> (TableStore -> Bool) -> TableStore -> Maybe TableStore
findStableState maxInterations f s
    | maxInterations <= 1 = Nothing
    | otherwise =  let
            xs = take maxInterations $ loopTableStore f s
            ys = zip xs (tail xs)
            zs = filter (\(a,b) -> storeToList a == storeToList b) ys
        in fst <$> headMay zs

findStableStateError :: Int -> (TableStore -> Bool) -> TableStore -> TableStore
findStableStateError maxInterations f s =
    let
        x = findStableState maxInterations f s
    in case x of
        Just x -> x
        Nothing -> error $ "findStableStateError would not stop after " <> show maxInterations <> " interations"

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

type TableElementType = Bool
type Width = Int
type Height = Int

-- x is column increases from left to right
-- y is row increses up to down
type Position = (Int,Int)
type Table = Map Position TableElementType

-- | The EnvT comonad is used to keep the original width and height
--   The element type is Bool, this way elements outside the original grid are considered to not have a paper roll because False is returned by peek. 
type TableStore = EnvT (Width,Height) (Store Position) TableElementType

listToTable :: [[TableElementType]] -> Table
listToTable xs =
    let
        numRows = length xs
        numCols = length $ head xs
    in Map.fromList $ concat $ imap (\j line -> imap (\i e -> ((i,j), e)) line) xs

tableToTableStore :: Width -> Height -> Table -> TableStore
tableToTableStore w h xs =
    let
        f (i,j) = fromMaybe False $ Map.lookup (i,j) xs
    in EnvT (w,h) $ store f (0,0)

listToStore :: [[TableElementType]] -> TableStore
listToStore xs = tableToTableStore w h $ listToTable xs where
    w = length $ head xs
    h = length xs

storeToCoordList :: TableStore -> [((Int,Int), TableElementType)]
storeToCoordList s = do
    let (w,h) = ask s
    i <- [0..(w-1)]
    j <- [0..(h-1)]
    return ((i,j), peek (i,j) s)

storeToTable :: TableStore -> Table
storeToTable s = Map.fromList $ storeToCoordList s

storeToList :: TableStore -> [[TableElementType]]
storeToList s = do
    let (w, h) = ask s
    j <- [0..(h-1)]
    return $ do
        i <- [0..(w-1)]
        return $ peek (i,j) s

listToString :: [[TableElementType]] -> String
listToString xs = unlines $ fmap (fmap (\x -> if x then '@' else '.')) xs

storeToString :: TableStore -> String
storeToString = listToString . storeToList

sumPair :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumPair (a,b) (c,d) = (a+c,b+d)

checkPaperNeighbours :: TableStore -> Bool
checkPaperNeighbours s =
    let
        nei = do
            dx <- [-1, 0, 1]
            dy <- [-1, 0, 1]
            guard $ (dx, dy) /= (0,0)
            return (dx, dy)
        adjacentSpaces = fmap (\p -> peeks (sumPair p) s) nei
        adjacentRolls = filter id adjacentSpaces
        hasRollAtFocus = extract s
        result = hasRollAtFocus && length adjacentRolls < 4
        traceString = show (pos s) <> ": " <> (if hasRollAtFocus then show result <> " " <> show (zip (sumPair (pos s) <$> nei) adjacentSpaces) else "No roll" )
    --in trace traceString result 
    in result

part1 :: ParsedType -> Int
part1 xs =
    let
        st = listToStore xs
        st' = extend checkPaperNeighbours st
        countRolls = length . concatMap (filter id) . storeToList
        result = countRolls st'
    -- in trace ("\n" <> storeToString st') result
    in result

removeRolls :: TableStore -> Bool
removeRolls s =
    let
        nei = do
            dx <- [-1, 0, 1]
            dy <- [-1, 0, 1]
            guard $ (dx, dy) /= (0,0)
            return (dx, dy)
        adjacentSpaces = fmap (\p -> peeks (sumPair p) s) nei
        adjacentRolls = filter id adjacentSpaces
        hasRollAtFocus = extract s
        result = hasRollAtFocus && length adjacentRolls >= 4
        traceString = show (pos s) <> ": " <> (if hasRollAtFocus then show result <> " " <> show (zip (sumPair (pos s) <$> nei) adjacentSpaces) else "No roll" )
    -- in trace traceString result 
    in result

part2 :: ParsedType -> Int
part2 xs =
    let
        countRolls = length . concatMap (filter id) . storeToList 
        st = listToStore xs
        st' = findStableStateError 1000 removeRolls st
        numRollsAtStart = countRolls st
        numRollsAtEnd = countRolls st'
        numRollsRemoved = numRollsAtStart - numRollsAtEnd 
    in numRollsRemoved
    --in trace ("\n" <> storeToString st') numRollsRemoved

































test1String = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@.\n"

test1parsed = parse parser "input" test1String

-- >>> test1parsed
-- Right [[False,False,True,True,False,True,True,True,True,False],[True,True,True,False,True,False,True,False,True,True],[True,True,True,True,True,False,True,False,True,True],[True,False,True,True,True,True,False,False,True,False],[True,True,False,True,True,True,True,False,True,True],[False,True,True,True,True,True,True,True,False,True],[False,True,False,True,False,True,False,True,True,True],[True,False,True,True,True,False,True,True,True,True],[False,True,True,True,True,True,True,True,True,False],[True,False,True,False,True,True,True,False,True,False]]

test1list = case test1parsed of
    Right xs -> xs
    Left _ -> [[]]

test1W = length $ head test1list
test1H = length test1list

-- >>> test1list    
-- [[False,False,True,True,False,True,True,True,True,False],[True,True,True,False,True,False,True,False,True,True],[True,True,True,True,True,False,True,False,True,True],[True,False,True,True,True,True,False,False,True,False],[True,True,False,True,True,True,True,False,True,True],[False,True,True,True,True,True,True,True,False,True],[False,True,False,True,False,True,False,True,True,True],[True,False,True,True,True,False,True,True,True,True],[False,True,True,True,True,True,True,True,True,False],[True,False,True,False,True,True,True,False,True,False]]

-- >>> T.pack (listToString test1list) == test1String
-- True

test1table = listToTable test1list

-- >>> Map.toList test1table
-- [((0,0),False),((0,1),True),((0,2),True),((0,3),True),((0,4),True),((0,5),False),((0,6),False),((0,7),True),((0,8),False),((0,9),True),((1,0),False),((1,1),True),((1,2),True),((1,3),False),((1,4),True),((1,5),True),((1,6),True),((1,7),False),((1,8),True),((1,9),False),((2,0),True),((2,1),True),((2,2),True),((2,3),True),((2,4),False),((2,5),True),((2,6),False),((2,7),True),((2,8),True),((2,9),True),((3,0),True),((3,1),False),((3,2),True),((3,3),True),((3,4),True),((3,5),True),((3,6),True),((3,7),True),((3,8),True),((3,9),False),((4,0),False),((4,1),True),((4,2),True),((4,3),True),((4,4),True),((4,5),True),((4,6),False),((4,7),True),((4,8),True),((4,9),True),((5,0),True),((5,1),False),((5,2),False),((5,3),True),((5,4),True),((5,5),True),((5,6),True),((5,7),False),((5,8),True),((5,9),True),((6,0),True),((6,1),True),((6,2),True),((6,3),False),((6,4),True),((6,5),True),((6,6),False),((6,7),True),((6,8),True),((6,9),True),((7,0),True),((7,1),False),((7,2),False),((7,3),False),((7,4),False),((7,5),True),((7,6),True),((7,7),True),((7,8),True),((7,9),False),((8,0),True),((8,1),True),((8,2),True),((8,3),True),((8,4),True),((8,5),False),((8,6),True),((8,7),True),((8,8),True),((8,9),True),((9,0),False),((9,1),True),((9,2),True),((9,3),False),((9,4),True),((9,5),True),((9,6),True),((9,7),True),((9,8),False),((9,9),False)]

-- >>> Map.lookup (2,0) test1table
-- Just True
test1store = listToStore test1list
-- >>> peek (0,0) test1store
-- False
-- >>> peek (2,0) test1store
-- True

-- Anything outside the grid is false
-- >>> peek (10000,0) test1store
-- False
