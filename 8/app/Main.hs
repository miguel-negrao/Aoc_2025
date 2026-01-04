module Main (main) where

import AoC
import qualified Data.Text.IO as TIO
import Text.Megaparsec (errorBundlePretty, parse)
import Data.List
import Data.Ord

main :: IO ()
main = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Right parsed -> do
            print $ take 3 $ sortOn Down $ part1UniqueLengths 1000 parsed
            print $ part1UniqueLengths 1000 parsed
            putStrLn $ "part1: " <> show (part1 parsed) <> "\n"
            --putStrLn $ "part2: " <> show (part2 input parsed) <> "\n"
        Left e -> putStrLn (errorBundlePretty e)
