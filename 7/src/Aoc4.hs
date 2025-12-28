{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

{--
notes:
part1
time: 
attempts: 
used chatgpt: 
notes: After reading my initial idea is to use comonads with tables. The evolution rules are if this cell is | put a | below, except if below is ^ put on |^|. Stop when state doesn't change on the whole board. At the end count the number ^ with | above. The question is going to be if my memoization scheme is really working and is good enough for the size of the table.

part2
time: 
attempts: 
used chatgpt: 
notes: 

--}

module Aoc4
    ( 
    
      part1
    , part2
    , TableStore
    , loopTableStore
    , slowLoop
    , tableToTableStore
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

memoizeTableStore :: TableStore -> TableStore
memoizeTableStore s =
    let (e,s') = runEnvT s
    in EnvT e (tab (Memo.pair Memo.integral Memo.integral) s')

loopTableStore :: (TableStore -> Bool) -> TableStore -> [TableStore]
loopTableStore f = iterate (memoizeTableStore . extend f . memoizeTableStore)

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

test1String = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@.\n"


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

-- part1 :: String -> Int
-- part1 xs =
--     let
--         st = listToStore xs
--         st' = extend checkPaperNeighbours st
--         countRolls = length . concatMap (filter id) . storeToList
--         result = countRolls st'
--     -- in trace ("\n" <> storeToString st') result
--     in result

part1 :: String -> Int
part1 xs = undefined

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

part2 :: String -> Int
part2 xs = undefined

    -- let
    --     countRolls = length . concatMap (filter id) . storeToList 
    --     st = listToStore xs
    --     st' = findStableStateError 1000 removeRolls st
    --     numRollsAtStart = countRolls st
    --     numRollsAtEnd = countRolls st'
    --     numRollsRemoved = numRollsAtStart - numRollsAtEnd 
    -- in numRollsRemoved
    -- --in trace ("\n" <> storeToString st') numRollsRemoved
