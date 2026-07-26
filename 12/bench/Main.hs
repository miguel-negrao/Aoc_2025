module Main (main) where

import AoC (parser, part1)
import qualified Data.Text.IO as TIO
import Test.Tasty.Bench (bench, defaultMain, nfIO)
import Text.Megaparsec (parse)

runPart1 = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Left _ -> error "no parse"
        Right parsed -> pure (part1 parsed)

main :: IO ()
main = defaultMain
    [ bench "part1" $ nfIO runPart1
    ]
