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
    test_input <- TIO.readFile "test_input"
    case parse parser "input" input of
        Left err -> putStrLn (errorBundlePretty err)
        Right parsed ->
            case parse parser "test_input" test_input of
                Left err -> putStrLn (errorBundlePretty err)
                Right test_parsed ->
                    defaultMain
                        [ 
                        bench "part1 without parsing" $ nf part1 parsed
                        , bench "part2 without parsing" $ nf part2 parsed
                        -- bench "test part2 without parsing" $ nf part2 test_parsed
                        , bench "part1 with parsing" $ nf (parseAndRun part1) input
                        --, bench "part2 with parsing" $ nf (parseAndRun part2) test_input
                        , bench "part2 with parsing" $ nf (parseAndRun part2) input
                        ]
