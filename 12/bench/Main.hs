module Main (main) where

import AoC (parser, part1)
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
                [ bench "part1 without parsing" $ nf part1 parsed
                , bench "part1 with parsing" $ nf (parseAndRun part1) input
                ]
