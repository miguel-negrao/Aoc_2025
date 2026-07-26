module Main (main) where

import AoC (part1, part2)
import qualified Data.ByteString.Char8 as B
import Test.Tasty.Bench (bench, defaultMain, nfIO)

runPart f = do
    input <- B.readFile "input"
    pure (f input)

main :: IO ()
main = defaultMain
    [ bench "part1" $ nfIO (runPart part1)
    , bench "part2" $ nfIO (runPart part2)
    ]
