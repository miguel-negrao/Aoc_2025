{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module AoC
    ( Parser
    , parser
    , part1
    , part2
    , ParsedType
    ) where

import Data.List ( tails )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor.Identity (Identity)
import Control.Applicative (Alternative)
import Control.Arrow
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L
import GHC.Natural
import Debug.Trace

{--
notes:
part1
time: 55m
used chatgpt: just for simple sintax questions like reminding me the name of <$ and <|> and how +| and |+ work. Also to check how to implement normalize with a single function (mod suffices).

part1 attempts: 1
part2 attempts: 4

notes:
I failed part2 because when the position is 0 it is tricky when advancing by 100, the corner cases are too hard.
I successed by just thinking what happens when moving just one click to the left or right, much simpler.
--}

data Direction = L | R deriving (Show, Read)
data Movement = Movement Direction Natural deriving Show

type Parser = Parsec Void Text

pNumber :: forall a. Read a => Parser a
pNumber = read <$> some digitChar

pDirection :: Parser Direction
pDirection = (L <$ char 'L') <|> (R <$ char 'R') 

pMovement :: Parser Movement
pMovement = do
    direction <- pDirection
    value <- pNumber
    newline
    return $ Movement direction value

type ParsedType = [Movement]

parser :: Parser [Movement]
parser = many pMovement

-- The dial starts by pointing at 50
-- the dial is in range [0,99]

startPosition :: Natural
startPosition = 50

maxValue :: Natural
maxValue = 99

directionToSignal :: Direction -> Int
directionToSignal L = -1
directionToSignal R = 1

safeDivisor :: Int
safeDivisor = 100

normalize :: Int -> Int -> Int
normalize divisor x
    | x < 0  = normalize divisor (x + divisor)
    | x >= divisor = normalize divisor (x - divisor)
    | otherwise    = x

-- after checking with chatgpt turns out mod actually does implement the above, should have tried that first.
-- Ah, actualy for part 2 normalize is better !
normalize2 :: Int -> Int -> Int
normalize2 divisor x = x `mod` divisor

getNumZeros :: [Natural] -> Int       
getNumZeros = length . filter (== 0)

-- part2
listPositions :: [Movement] -> [Natural]
listPositions = scanl moveDial startPosition
    where
        moveDial currentPosition (Movement direction value) = 
            fromIntegral $ normalize safeDivisor $ fromIntegral currentPosition + (directionToSignal direction * fromIntegral value)

--record crossing zero
normalize3 :: Int -> Int -> (Int, Natural)
normalize3 divisor y = normalize3Internal divisor (y, 0) where
    normalize3Internal divisor (x, n)
        | x < 0  = normalize3Internal divisor (x + divisor, n+1)
        | x >= divisor = normalize3Internal divisor (x - divisor, n+1)
        | otherwise    = (x, if x == 0 && x == y then n + 1 else n)

-- Wrong.
listPositions2 :: [Movement] -> [(Natural, Natural)]
listPositions2 = scanl moveDial (startPosition, 0)
    where
        moveDial (currentPosition, count) (Movement direction rotationAmount) = (nextPosition, nextCount) where
            rotation = (directionToSignal direction * fromIntegral rotationAmount)
            (nextPosition, n) = first fromIntegral $ normalize3 safeDivisor $ fromIntegral currentPosition + rotation
            nextCount = if currentPosition == 0 && rotation < 0 then trace ("listPositions2 "<>show currentPosition<>" "<>show rotation<>" "<>show n) count + n - 1 else count + n

-- second attempt at part 2, single movement left or right
listPositions3 :: [Movement] -> [(Natural, Natural)]
listPositions3 = scanl moveDial (startPosition, 0)
    where
        moveDial :: (Natural, Natural) -> Movement -> (Natural, Natural)
        moveDial (pos, n) (Movement _ 0)              = (pos, n) -- base case
        moveDial (0, n)   (Movement L rotationAmount) = moveDial (99, n)      (Movement L (rotationAmount -1)) 
        moveDial (1, n)   (Movement L rotationAmount) = moveDial (0, n + 1)   (Movement L (rotationAmount -1)) 
        moveDial (pos, n) (Movement L rotationAmount) = moveDial (pos - 1, n) (Movement L (rotationAmount-1))
        moveDial (99, n)  (Movement R rotationAmount) = moveDial (0, n + 1)   (Movement R (rotationAmount -1)) 
        moveDial (pos, n) (Movement R rotationAmount) = moveDial (pos + 1, n) (Movement R (rotationAmount-1))

part1 :: [Movement] -> Int
part1 movements = getNumZeros $ listPositions movements

part2:: [Movement] -> Int
part2 movements = fromIntegral $ numZeros2 where
    numZeros2 = snd $ last positions2
    positions2 = listPositions3 movements
