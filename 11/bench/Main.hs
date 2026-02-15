module Main (main) where

import AoC
import qualified Data.Text.IO as TIO
import Test.Tasty.Bench (bench, defaultMain, nf)
import Text.Megaparsec (errorBundlePretty, parse)


parseAndRun f input = case parse parser "input" input of
        Left err -> error "no parse"
        Right parsed -> f parsed


main :: IO ()
main = do
    input <- TIO.readFile "input"
    let 
        p2_intmap = dft2_1 svr . convertGraph2
        p2_vector = dft2_2 svr . convertGraph2'
    case parse parser "input" input of
        Left err -> putStrLn (errorBundlePretty err)
        Right parsed ->
            defaultMain
                [ bench "part1 without parsing" $ nf part1 parsed
                , bench "part2 without parsing p2_intmap" $ nf p2_intmap parsed
                , bench "part2 without parsing p2_vector" $ nf p2_vector parsed
                , bench "part1 with parsing" $ nf (parseAndRun part1) input
                , bench "part2 with parsing p2_intmap" $ nf (parseAndRun p2_intmap) input
                , bench "part2 with parsing p2_vector" $ nf (parseAndRun p2_vector) input
                ]
