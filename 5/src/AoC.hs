{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{- HLINT ignore "Unused LANGUAGE pragma" -}

{--
part1
time: 18m
attempts: 1
used chatgpt: no
notes: extremely easy...

part2
time:
attempts: 1
used chatgpt: no
notes: first (naive) attempt crashes program. Nice, I like these ones ! 

--}

module AoC
    ( Parser
    , parser
    , part1
    , part2
    ) where

import Data.List (tails, subsequences, inits, nub, sort, (\\))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace (trace)
import GHC.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, char, newline)
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

-- 3-5
-- 10-14
-- 16-20
-- 12-18

-- 1
-- 5
-- 8
-- 11
-- 17
-- 32

type Parser = Parsec Void Text
type Interval = (Natural, Natural)
type IngredientRanges = [Interval]
type AvailableIngredients = [Natural]
data Database = Database IngredientRanges AvailableIngredients deriving (Show, Eq, Read)
type ParsedType = Database

pNatural :: Parser Natural
pNatural = read <$> some digitChar

pRanges :: Parser IngredientRanges
pRanges = some $ do
    a <- pNatural
    char '-'
    b <- pNatural
    newline
    return (a,b)

pAvailable :: Parser AvailableIngredients
pAvailable = some $ pNatural <* newline

parser :: Parser ParsedType
parser = do
    ranges <- pRanges
    newline
    Database ranges <$> pAvailable

tString = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32\n"

tDatabase = parse parser "input" tString

-- >>> tDatabase
-- Right (Database [(3,5),(10,14),(16,20),(12,18)] [1,5,8,11,17,32])

isFresh :: IngredientRanges -> Natural -> Bool
isFresh ranges x =
    let
        f (a,b) = a <= x && x <= b
    in any f ranges

countPred :: (a -> Bool) -> [a] -> Int
countPred f = length . filter id . fmap f

part1 :: ParsedType -> Int
part1 (Database ranges available) = countPred (isFresh ranges) available

-- | blows up on the real input
part2v1 :: ParsedType -> Int
part2v1 (Database ranges _) = Set.length $ Set.unions $ fmap (\(a,b) ->  Set.fromList [a..b]) ranges

-- | we assume 
--  a < b and c < d
intervalUnion :: Interval -> Interval -> [Interval]
intervalUnion (a,b) (c,d)
    | b < c = [(a,b), (c,d)]                --  [a,b] (c,d) 
    | d < a = [(a,b), (c,d)]                --  (c,d) [a,b]
    | a < c && d < b = [(a,b)]              --  [a (c,d) b]
    | c < a && b < d = [(c,d)]              --  (c [a,b] d)
    | a < c && c < b && b < d = [(a,d)]     --  [a (c, b] d)
    | c < a && a < d && d < b = [(c,b)]     --  [c (a, d] b)
    | a == c                  = [(a, max b d)]
    | b == d                  = [(min a c, b)]
    | otherwise = error (show ((a,b),(c,d)))
        -- any missing ?

intervalUnion' (a,b) (c,d) = trace ("intervalUnion " <> show (a,b) <> " " <> show (c,d) <> " = " <> show result) result where result = intervalUnion (a,b) (c,d)

intervalUnionsV1 [] = []
intervalUnionsV1 (x:xs)= sort $ nub $ foldl f [x] xs where
    f intervals int = concatMap (intervalUnion int) intervals

-- doesn't work 
intervalUnionsv2 :: IngredientRanges -> IngredientRanges
intervalUnionsv2 xs =
    let
        pairs = filter (\x -> length x == 2) $ subsequences xs
        f [a,b] = intervalUnion a b
    in sort $ nub $ concatMap f pairs

intervalUnionsV3 :: IngredientRanges -> IngredientRanges
intervalUnionsV3 = foldl f [] where
    f :: IngredientRanges -> Interval -> IngredientRanges
    f xs (a1,b1)
        | not $ null containing = trace ("do nothing "<> show xs <>" (a1,b1) = " <> show (a1,b1))xs
        | otherwise = trace ("\nxs = " <> show xs <> " (a1,b1) = " <> show (a1,b1) <> " (a3,b3) = " <> show (a3,b3)<>" intersectingLeft: "<> show intersectingLeft <> "intersectingRight: " <> show intersectingRight <> " withoutInters: " <> show withoutInters<>"\n") (a3,b3):withoutInters
        where
            containing = filter (\(a2, b2) -> a2 <= a1 && b1 <= b2) xs
            withoutContained = filter (\(a2,b2) -> not (a1 <= a2 && b2 <= b1)) xs 
            intersectingLeft = filter (\(a2,b2) -> a2 < a1 && a1 < b2 && b2 <= b1) withoutContained
            intersectingRight = filter (\(a2,b2) -> a1 <= a2 && a2 < b1 && b1 < b2) withoutContained
            withoutInters = (withoutContained \\ intersectingLeft) \\ intersectingRight
            a3 = case intersectingLeft of
                [] -> a1
                xs -> minimum $ fmap fst xs
            b3 = case intersectingRight of
                [] -> b1
                xs -> maximum $ fmap snd xs
            

-- >>> intervalUnionsV3 [(3,5),(10,14),(16,20),(12,18)]
-- [(10,20),(3,5)]

-- | idea calculte unions of intervals using the intervals and not the individual elements
part2v2 :: ParsedType -> Int
part2v2 (Database ranges _) =
    let
        finalIntervals :: IngredientRanges
        finalIntervals = intervalUnionsV3 ranges
        f (a,b) = fromIntegral $ b - a + 1
    in sum $ fmap f finalIntervals

part2 :: ParsedType -> Int
part2 = part2v2














untilStable :: Eq a => Int -> (a -> a) -> a -> a
untilStable 0 _ _ = error "untilStable: reached max iterations"
untilStable n f a
    | fa == a = a
    | otherwise = untilStable (n-1) f a
    where
        fa = f a
