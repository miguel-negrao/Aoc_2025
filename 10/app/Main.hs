module Main (main) where

import AoC
import qualified Data.Text.IO as TIO
import Text.Megaparsec (errorBundlePretty, parse)
import Data.List
import Data.Ord
import Data.SBV

main :: IO ()
main = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Right parsed -> do
            putStrLn $ "part1: " <> show (part1 parsed) <> "\n"
            --putStrLn $ "part2: " <> show (part2 parsed) <> "\n"
            part2v2 parsed >>= print
        Left e -> putStrLn (errorBundlePretty e)
