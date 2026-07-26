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

import Control.Error (Script, runScript, scriptIO, throwE, tryRead)
import qualified Data.List as List
import Data.List ( tails, subsequences )
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
import Data.List (inits)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Text.Read (Lexeme(Char))

{--
notes:
part1
time: m
used chatgpt: no

part2
time: 7Dez - 22Dec
used chatgpt: no

part1 attempts: 1
part2 attempts: 4 ?

notes:
part1: the trick was using zip with tails with one dropped. This aligns each digit with the subsequent digits. Then is just using the list monad. Could have just used subsequences...
part2: this was quite hard for me. I took quite a couple of tries to get the right answer.
--}

type Parser = Parsec Void Text

pLine :: Parser String
pLine = do
    digits <- many digitChar
    newline
    return digits

type ParsedType = [String] 

parser :: Parser ParsedType
parser = many pLine

-- https://en.wikipedia.org/wiki/Subsequence

-- part 1

--
maxJoltage :: String -> Int
maxJoltage xs = maximum ys where
    _:t = tails xs
    -- pair each digit with the digits that come after
    zs = zip xs t
    ys = f <$> zs 
    -- generate all combinations with the start digit plus another digit that comes after then pick maximum
    f (digit, nextDigits) = if null joltages then 0 else maximum joltages where
        joltages = do
            anotherDigit <- nextDigits
            return $ read [digit, anotherDigit]

part1 :: [String] -> Int
part1 = sum . fmap maxJoltage

-- part 2

-- 12 digits

-- It seems this might take too long.
-- for each line subsequences needs to calculate 2^100 possibilities.
maxJoltage2v1 :: String -> Int
maxJoltage2v1 xs = trace ("result: "<>show result) result where
    result = maximum ys
    ys :: [Int]
    ys = read <$> filter (\xs -> length xs == 12) (subsequences xs)

-- another ideia: remove all the ones check if there are still more then 12 chars left, if so remove all the 2... etc. when you get to a situation with less than 12 backtrack and calculate subsequences on those.
-- later I wrote:
-- 123222222222111111119
-- that would remove all the 1 leaving 232222222229 but if had removed the 2 at the start it would work better. So nope.

-- next remove some of the smallest digit still in there ? we know that this sequences has size n >= 12 but if we remove the smallest digit it has size m < 12. 
-- the number of smaller digits is p.
-- Nope this doesn't work
-- "3533573347335434353333443"
-- correct: 775453333443
-- my algo: 557475445443
-- Another algorithm:
-- find the largest digit
-- remove occurrences of any other digit until at the start you only have that digit and there are no more of that digit ot the sequence has size 12
-- "3533573347335434353333443" -> "77335434353333443" (l = 17)
-- do the same for the next lower digit
-- "77335434353333443" -> "775434353333443"
-- that's not it, let's try again
maxJoltage2v2 :: String -> Int
maxJoltage2v2 xs = maximum ys where
    ys :: [Int]
    ys = read <$> filter (\xs -> length xs == 12) (subsequences (trace ("zs: " <> show ks) ks))
    ks = removeFirstOccurences zs smallest $ fromIntegral (lzs - 12)
    smallest :: Char
    smallest = head $ show $ minimum ((read :: String -> Int) . pure <$> zs)
    lzs = length zs
    zs = last $ trace ("rs: " <> show rs <> "sizes: " <> show (length <$> rs)) rs
    rs =  filter (\xs -> length xs >= 12) $ f <$> inits ['1','2','3','4','5','6','7','8','9']
    f zs = filter (`notElem` zs) xs

removeFirstOccurences :: Eq a => [a] -> a -> Natural -> [a]
removeFirstOccurences [] _ _ = []
removeFirstOccurences xs _ 0 = xs
removeFirstOccurences (x:xs) a n
    | x == a = removeFirstOccurences xs a (n - 1)
    | otherwise = x:removeFirstOccurences xs a n


-- >>> removeFirstOccurences "123123982018731023601123123t1935" '1' 100
-- "23239820873023602323t935"


--remove all 1s until there are no more or the lenght is 12, then 2s, then 3s, etc...
-- 234234234234278
-- 343434234278 mine removed 3x 2
-- 434234234278 correct
-- ok so this is not the right algorithm
maxJoltage2v4 :: String -> Int
maxJoltage2v4 zs = trace ("maxJoltage2v4 zs = " <> result) (read result) where
    result = go [] zs '1'
    succ :: Char -> Char
    succ m = head (show (read [m] + 1))
    go :: String -> String -> Char -> String
    go _ [] _ = [] -- never gonna happen
    go ys (x:xs) m
        | length (ys ++ (x:xs)) == 12 = ys ++ (x:xs)
        | m `notElem` (x:xs) = go [] (ys ++ (x:xs)) (succ m)
        | otherwise = if x == m then go ys xs m else go (ys++[x]) xs m 

-- 1. pick the highest number x.
-- 2. remove all smaller then x while there is still x. 
-- 3. if there is no more x, pick the hightest number now. 
-- doesn't work. If the largest are at the end then it removes too much as the start.
-- I wonder if there even is a clean algorithm of this type...
maxJoltage2v5 :: String -> Int
maxJoltage2v5 zs = trace ("maxJoltage2v4 zs = " <> result) (read result) where
    max = maximum zs
    result = go [] zs max
    go :: String -> String -> Char -> String
    go _ [] _ = [] -- never gonna happen
    go ys (x:xs) m
        | length (ys ++ (x:xs)) == 12 = ys ++ (x:xs)
        | m `notElem` (x:xs) = go ys (x:xs) (maximum (x:xs))
        | otherwise = if x < m then go ys xs m else go (ys++[x]) xs m 

-- >>> maxJoltage2v5 "234234234234278"
-- 234234234278

--  || |
-- "234234234234278"
-- insights:
-- How can I get a digit to the be the first digit ?
-- it cannot be in the last 11 digits, because even if I delete nothing after that digit I need still 12 digits.
-- pick the highest digit in all digits except last 11, delete everything before the first occurrence of that digit.
-- select here 2342 keep 34234234278
-- largest is 4 delete all before
-- 42 34234234278 and now how to proceed ? now keep the 4.
-- restart with everything after the 4 (2 34234234278) and reapply but considering last 10. and keep going.
-- when you hit 0 just pick the maximum of what is left
-- Finally, it works !
maxJoltage2v6 :: String -> String
maxJoltage2v6 xs = final where
    --trace ("maxJoltage2v6 " <> xs <> " = " <> final) final where
    final = go xs 11 
    go :: String -> Int -> String
    go xs 0 = [maximum xs] 
    go xs n = result where
        -- trace ("go "<> xs <> " " <> (show n) <> " = " <> result) result where
        l = length xs
        r = l - n
        left = take r xs
        right = drop r xs
        max = maximum left
        (y:ys) = dropWhile (/= max) left
        result = y : go (ys ++ right) (n - 1)

part2 :: [String] -> Int
part2 = sum . fmap (read . maxJoltage2v6)

test :: IO ()
test = do
    testInput :: Text <- TIO.readFile "test_input"
    case parse parser "file" testInput of
        Right parsed -> do
            let
                part1CorrectAnswer = 357
                part1MyAnswer = part1 parsed
                part2CorrectAnswer = 3121910778619
                part2MyAnswer = part2 parsed
                txt :: Text
                txt = "Parsed: " +| show parsed |+
                        "\npart1 correct answer: " +|| part1CorrectAnswer ||+
                        "\npart1 my answer:      " +|| part1MyAnswer ||+
                        "\npart1 test correct:   " +|| part1MyAnswer ==  part1CorrectAnswer ||+
                        "\npart2 correct answer: " +|| part2CorrectAnswer ||+
                        "\npart2 my answer:      " +|| part2MyAnswer ||+
                        "\npart2 test correct:   " +|| part2MyAnswer ==  part2CorrectAnswer ||+"\n"
            TIO.putStr txt
        Left e -> pPrint e
    input :: Text <- TIO.readFile "input"
    case parse parser "file" input of
        Right parsed -> do
            let
                txt :: Text
                txt =
                       "\npart1: " +|| part1 parsed ||+
                       "\npart2: " +|| part2 parsed ||+ "\n"
            TIO.putStr txt
        Left e -> pPrint e
