module Main (main) where

import AoC (parser, part1, part2)
import qualified Data.Text.IO as TIO
import Test.Tasty.Bench (bench, defaultMain, nf)
import Text.Megaparsec (errorBundlePretty, parse)


parseAndRun f input = case parse parser "input" input of
        Left err -> error "no parse"
        Right parsed -> f parsed

main :: IO ()
main = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Left err -> putStrLn (errorBundlePretty err)
        Right parsed ->
            defaultMain
                [ bench "part1" $ nf part1 parsed
                , bench "part2" $ nf part2 parsed
                , bench "part1 with parsing" $ nf (parseAndRun part1) input
                , bench "part2 with parsing" $ nf (parseAndRun part2) input
                ]
