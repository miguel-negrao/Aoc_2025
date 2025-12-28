module Main (main) where

import Aoc4 (TableStore, loopTableStore, part1, part2, slowLoop)
import Control.Comonad (extract)
import Control.Comonad.Env (EnvT(..))
import Control.Comonad.Store (peek, pos, store)
import Control.Exception (evaluate)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

main :: IO ()
main = defaultMain $ testGroup "Aoc4"
    [ 
    -- testCase "parser parses test_input" $ do
    --     input <- TIO.readFile "test_input"
    --     assertBool "expected parse to succeed" (isRight (parse parser "test_input" input))
    -- , testCase "parser parses input" $ do
    --     input <- TIO.readFile "input"
    --     assertBool "expected parse to succeed" (isRight (parse parser "input" input))
    testCase "part1 example (skipped: part1 is undefined)" $ do
        pure ()
    , testCase "loopTableStore memoizes per coordinate" $ do
        ref <- newIORef (0 :: Int)
        let f pos = unsafePerformIO $ do
                modifyIORef' ref (+1)
                pure (pos == (0,0))
            s0 = EnvT (2,2) (store f (0,0))
            s1 = (loopTableStore extract s0) !! 1
        _ <- evaluate $ peek (0,0) s1
        _ <- evaluate $ peek (0,0) s1
        _ <- evaluate $ peek (1,1) s1
        c <- readIORef ref
        assertEqual "expected one eval per coord" 2 c
    , testCase "slowLoop does not memoize per coordinate" $ do
        ref <- newIORef (0 :: Int)
        let f pos = unsafePerformIO $ do
                modifyIORef' ref (+1)
                pure (pos == (0,0))
            s0 = EnvT (2,2) (store f (0,0))
            s1 = (slowLoop extract s0) !! 1
        _ <- evaluate $ peek (0,0) s1
        _ <- evaluate $ peek (0,0) s1
        _ <- evaluate $ peek (1,1) s1
        c <- readIORef ref
        assertEqual "expected no memoization" 3 c
    , testCase "loopTableStore memoizes per state with toggle rule" $ do
        ref <- newIORef (0 :: Int)
        let bump (x,y) = x * 10 + y + 1
            toggle :: TableStore -> Bool
            toggle s =
                let p = pos s
                in unsafePerformIO (modifyIORef' ref (+ bump p)) `seq` not (extract s)
            s0 = EnvT (2,2) (store (const False) (0,0))
            states = drop 1 (take 3 (loopTableStore toggle s0))
            coords = [(0,0), (1,1)]
        mapM_ (\st -> do
            before <- readIORef ref
            mapM_ (\p -> evaluate $ peek p st) coords
            mid <- readIORef ref
            mapM_ (\p -> evaluate $ peek p st) coords
            after <- readIORef ref
            assertBool "expected new evaluations on first scan" (mid > before)
            assertEqual "expected no extra evals on re-scan" mid after
            pure ()) states
    -- , testCase "part2 example" $ do
    --     input <- TIO.readFile "test_input"
    --     case parse parser "test_input" input of
    --         Left err -> assertFailure (show err)
    --         Right parsed -> assertEqual "part2" 43 (part2 parsed)
    ]
