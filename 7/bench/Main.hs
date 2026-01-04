module Main (main) where

import AoC (part1, part2)
import qualified Data.Text.IO as TIO
import Test.Tasty.Bench (bench, defaultMain, nf)
import Text.Megaparsec (errorBundlePretty, parse)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    input <- readFile "input"
    --input2 <- readFile "input2"
    --testInput <- readFile "test_input"
    input <- B.readFile "input"
    defaultMain
        [-- bench "part1" $ nf part1 testInput
        bench "part2" $ nf part2 input, 
        bench "part2" $ nf part2 input 
        --bench "part2" $ nf part2 input
        ]   
