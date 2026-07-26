module Main (main) where

import AoC (parser, part1v4, part2v2)
import qualified Data.Text.IO as TIO
import Test.Tasty.Bench (bench, defaultMain, nfIO)
import Text.Megaparsec (parse)

runPart1 = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Left _ -> error "no parse"
        Right parsed -> part1v4 parsed

runPart2 = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Left _ -> error "no parse"
        Right parsed -> part2v2 parsed

main :: IO ()
main = defaultMain
    [ bench "part1" $ nfIO runPart1
    , bench "part2" $ nfIO runPart2
    ]
