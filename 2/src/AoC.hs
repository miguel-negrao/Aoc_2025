{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Error (Script, runScript, scriptIO, throwE, tryRead)
import Data.List ( tails )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor.Identity (Identity)
import Control.Applicative (Alternative)
import Control.Arrow
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)
import GHC.Natural
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Foldable as Set


{--
notes:
part1
start: 2025-12-06 ??
end:   2025-12-06 ??
time:  ??m
used chatgpt: 

part1
start: 2025-12-06 ??
end:   2025-12-07 ??
time:  ??m
used chatgpt:

part1 attempts: 
part2 attempts: 

notes:

--}

{--
notes:
part1
time:  43m
used chatgpt: no

part1
time:  38m
used chatgpt: checked if a chunk function already existed in Data.List

part1 attempts: 1
part2 attempts: 2

notes:
part1 was easy.
part2 was not hard, the first try I thought the pattern couldn't be of 1 digit long, thought it had to be at least 2.
--}

type Parser = Parsec Void Text

pNumber :: forall a. Read a => Parser a
pNumber = read <$> some digitChar

pRange :: Parser (Natural, Natural)
pRange = do
    x <- pNumber
    char '-'
    y <- pNumber
    return (x,y)

pList :: Parser [(Natural, Natural)]
pList = sepBy pRange (char ',')

-- part 1

-- lists not allowed: a group of m digits (m >= 1) repeated twice
-- any id not allowes must have lenght divisible by 2 and when spit in two the first half must equal the second half
isInvalid1 :: Natural -> Bool
isInvalid1 n = even l && a == b where
    t = show n
    l = length t
    (a,b) = splitAt (l `div` 2) t

check :: (Natural -> Bool) -> [(Natural, Natural)] -> Natural
check g xs = sum $ fmap sum ys where
    ys = fmap f xs
    f :: (Natural, Natural) -> [Natural]
    f (a,b) = filter g [a..b]

part1 :: [(Natural, Natural)] -> Natural
part1 = check isInvalid1

-- part 2
-- >>> chunkList 4 [1..10] 
-- [11,12,13,14,15,16,17,18,19,20,21,22]

chunkList :: Int -> [a] -> [[a]]
chunkList blockSize xs = go xs [] where
    go xs ys = if length ws <= blockSize then reverse (ws:zs:ys) else go ws (zs:ys) where
        (zs, ws) = splitAt blockSize xs

-- lists not allowed: a group of m digits (m >= 1) repeated n times (n >= 2)
-- n can only go up to the lenght of the string so I only have to check 2 <= n <= length list and only for length divisible by n

isInvalid2 :: Natural -> Bool
isInvalid2 n = l > 1 && isInvalid' where -- (isInvalid' && trace (show n) True) where
    isInvalid' = any isRepeatedPattern [(1::Int)..l]
    t = show n
    l = length t
    -- I split the string in chunks and if the chunks are all equal then it is invalid
    -- when blockSize is not a divisor of length t it will be trivially false as the last list is not even the same size as the others.
    -- converting to set keeps only the non duplicated values
    isRepeatedPattern blockSize = length z >= 2 {- at least 2 repetitions -} && x where
        --if x then trace (show y) x else x where 
        x = Set.length y == 1
        y = Set.fromList z 
        z = chunkList blockSize t

part2 :: [(Natural, Natural)] -> Natural
part2 = check isInvalid2

main :: IO ()
main = do
    testInput :: Text <- TIO.readFile "test_input"
    case parse pList "file" testInput of
        Right ranges -> do
            let
                part1TestAnswer = 1227775554
                part2TestAnswer = 4174379265
                part2Test = part2 ranges
                txt :: Text
                txt = "Parsed: " +| show ranges |+
                       "\npart1 test: " +|| part1 ranges ||+
                       "\npart1 test correct: " +|| part1 ranges ==  part1TestAnswer ||+
                       "\npart2 test my value:      " +|| part2Test ||+
                       "\npart2 test correct value: " +|| part2TestAnswer ||+
                       "\npart2 test correct: " +|| part2Test ==  part2TestAnswer ||+"\n"
            TIO.putStr txt
        Left e -> pPrint e
    input :: Text <- TIO.readFile "input"
    case parse pList "file" input of
        Right ranges -> do
            let
                txt :: Text
                txt =
                       "\npart1: " +|| part1 ranges ||+
                       "\npart2: " +|| part2 ranges ||+ "\n"
            TIO.putStr txt
        Left e -> pPrint e
