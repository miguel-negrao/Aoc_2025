module Main (main) where

import Aoc4 (parser, part1, part2)
import qualified Data.Text.IO as TIO
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
    input <- readFile "input"
    putStrLn $ "part1: " <> show (part1 input) <> "\n"
    -- putStrLn $ "part2: " <> show (part2 parsed) <> "\n"
    -- case parse parser "input" input of
    --     Right parsed -> do
    --         putStrLn $ "part1: " <> show (part1 parsed) <> "\n"
    --         putStrLn $ "part2: " <> show (part2 parsed) <> "\n"
    --     Left e -> putStrLn (errorBundlePretty e)
