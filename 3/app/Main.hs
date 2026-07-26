module Main (main) where

import AoC (parser, part1, part2)
import qualified Data.Text.IO as TIO
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
    input <- TIO.readFile "input"
    case parse parser "input" input of
        Right parsed -> do
            putStrLn $ "part1: " <> show (part1 parsed)
            putStrLn $ "part2: " <> show (part2 parsed)
        Left err -> putStrLn (errorBundlePretty err)
