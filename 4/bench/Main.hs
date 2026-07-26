module Main (main) where

import Aoc4 (parser, part1, part2)
import qualified Data.Text.IO as TIO
import Test.Tasty.Bench (bench, defaultMain, nfIO)
import Text.Megaparsec (parse)

runPart f = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Left _ -> error "no parse"
        Right parsed -> pure (f parsed)

main :: IO ()
main = defaultMain
    [ bench "part1" $ nfIO (runPart part1)
    , bench "part2" $ nfIO (runPart part2)
    ]
