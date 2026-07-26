module Main (main) where

import AoC (parser, part1, part2)
import qualified Data.Text.IO as TIO
import Test.Tasty.Bench (bench, defaultMain, nfIO)
import Text.Megaparsec (parse)

runPart1 :: IO Int
runPart1 = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Left _ -> error "no parse"
        Right parsed -> pure (part1 parsed)

runPart2 :: IO Int
runPart2 = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Left _ -> error "no parse"
        Right parsed -> pure (part2 input parsed)

main :: IO ()
main = defaultMain
    [ bench "part1" $ nfIO runPart1
    , bench "part2" $ nfIO runPart2
    ]
