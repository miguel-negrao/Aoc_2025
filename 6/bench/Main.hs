module Main (main) where

import AoC (parser, part1, part2)
import qualified Data.Text.IO as TIO
import Test.Tasty.Bench (bench, defaultMain, nf)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Left err -> putStrLn (errorBundlePretty err)
        Right parsed ->
            defaultMain
                [ bench "part1" $ nf part1 parsed
                , bench "part2" $ nf (part2 input) parsed
                ]
