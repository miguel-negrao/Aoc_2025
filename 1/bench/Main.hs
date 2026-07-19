module Main (main) where

import AoC
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Tasty.Bench (bench, defaultMain, nf)
import Text.Megaparsec (parse)

parseAndRun :: (ParsedType -> Int) -> Text -> Int
parseAndRun f input = case parse parser "input" input of
    Left _ -> error "no parse"
    Right parsed -> f parsed

main :: IO ()
main = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Left _ -> error "no parse"
        Right parsed ->
            defaultMain
                [ bench "part1 without parsing" $ nf part1 parsed
                , bench "part2 without parsing" $ nf part2 parsed
                , bench "part1 with parsing" $ nf (parseAndRun part1) input
                , bench "part2 with parsing" $ nf (parseAndRun part2) input
                ]
