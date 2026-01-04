module Main (main) where

import AoC (part1, part2)
import qualified Data.Text.IO as TIO
import Text.Megaparsec (errorBundlePretty, parse)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    --input <- readFile "input2"
    input <- B.readFile "input"
    putStrLn $ "part1: " <> show (part1 input)
    putStrLn $ "part2: " <> show (part2 input)
