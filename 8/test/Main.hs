module Main (main) where

import AoC
import Data.Either (isRight, fromRight)
import qualified Data.Text.IO as TIO
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Text.Megaparsec (parse)
import Test.QuickCheck (Gen, Property, choose, forAll, listOf, (==>), shuffle)
import Numeric.Natural (Natural)
import qualified Data.Set as Set
import Data.List
import Math.Combinat.Sets (combine)
import qualified Math.Combinat.Sets as Sets
import qualified Data.Map.Strict as Map

chooseSets :: Int -> [a] -> [[a]]
chooseSets = Sets.choose

type V3 = (Int, Int, Int)

genV3 :: Gen V3
genV3 = (,,) <$> genCoord <*> genCoord <*> genCoord
  where
    genCoord = choose (-50, 50)

genV3List :: Gen [V3]
genV3List = listOf genV3

distanceInt :: Num a => (a, a, a) -> (a, a, a) -> a
distanceInt (x1,y1,z1) (x2,y2,z2) = dx*dx + dy*dy + dz*dz
  where
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2

prop_pairsLength :: Property
prop_pairsLength = forAll genV3List $ \xs ->
    let pairs = getPairsOrderedByDistance xs
    in length pairs == length (chooseSets 2 xs)

prop_pairsSortedByDistance :: Property
prop_pairsSortedByDistance = forAll genV3List $ \xs ->
    let pairs = getPairsOrderedByDistance xs
        ds = fmap (uncurry distanceInt) pairs
    in and (zipWith (<=) ds (drop 1 ds))

prop_pairsElementsFromInput :: Property
prop_pairsElementsFromInput = forAll genV3List $ \xs ->
    let pairs = getPairsOrderedByDistance xs
    in all (\(a,b) -> a `elem` xs && b `elem` xs) pairs

prop_distanceSymmetry :: Property
prop_distanceSymmetry = forAll genV3 $ \a ->
    forAll genV3 $ \b ->
        distanceInt a b == distanceInt b a

prop_distanceZero :: Property
prop_distanceZero = forAll genV3 $ \a ->
    distanceInt a a == 0

prop_pairsEmptyWhenSmall :: Property
prop_pairsEmptyWhenSmall = forAll genV3 $ \a ->
    let xs0 = []
        xs1 = [a]
    in null (getPairsOrderedByDistance xs0) && null (getPairsOrderedByDistance xs1)

prop_groupsEmptyWhenZero :: Property
prop_groupsEmptyWhenZero = forAll genV3List $ \xs ->
    null (part1Groups 0 xs)

prop_groupsTotalSizeBounded :: Property
prop_groupsTotalSizeBounded = forAll genV3List $ \xs ->
    forAll (choose (0, length (getPairsOrderedByDistance xs))) $ \n ->
        let total = sum (Set.size <$> part1Groups n xs)
        in total <= length xs

prop_uniqueLengthsWithinBounds :: Property
prop_uniqueLengthsWithinBounds = forAll genV3List $ \xs ->
    forAll (choose (0, length (getPairsOrderedByDistance xs))) $ \n ->
        let maxLen = length xs
        in all (\k -> k >= 1 && k <= maxLen) (part1UniqueLengths n xs)

normalizeGroups :: [Set.Set V3] -> [[V3]]
normalizeGroups = sort . fmap (sort . Set.toList)

prop_permutationInvariantWhenUniqueDistances :: Property
prop_permutationInvariantWhenUniqueDistances = forAll genV3List $ \xs ->
    let pairs = getPairsOrderedByDistance xs
        ds = fmap (uncurry distanceInt) pairs
        uniqueDistances = length ds == length (nub ds)
    in uniqueDistances ==>
        forAll (choose (0, length pairs)) $ \n ->
            forAll (shuffle xs) $ \shuffled ->
                normalizeGroups (part1Groups n xs) == normalizeGroups (part1Groups n shuffled)

neighborsMap :: [(V3, V3)] -> Map.Map V3 [V3]
neighborsMap = foldl' insertEdge Map.empty
  where
    insertEdge m (a,b) =
        Map.insertWith (++) a [b] $ Map.insertWith (++) b [a] m

reachableFrom :: V3 -> Map.Map V3 [V3] -> Set.Set V3
reachableFrom start neighbors = go Set.empty [start]
  where
    go visited [] = visited
    go visited (x:xs)
        | x `Set.member` visited = go visited xs
        | otherwise =
            let next = Map.findWithDefault [] x neighbors
            in go (Set.insert x visited) (next ++ xs)

prop_groupsAreConnected :: Property
prop_groupsAreConnected = forAll genV3List $ \xs ->
    forAll (choose (0, length (getPairsOrderedByDistance xs))) $ \n ->
        let pairs = take n (getPairsOrderedByDistance xs)
            neighbors = neighborsMap pairs
            groups = part1Groups n xs
            groupOk g = case Set.toList g of
                [] -> True
                (start:_) -> reachableFrom start neighbors == g
        in all groupOk groups

prop_maxGroupSizeMonotone :: Property
prop_maxGroupSizeMonotone = forAll genV3List $ \xs ->
    forAll (choose (0, length (getPairsOrderedByDistance xs))) $ \n1 ->
    forAll (choose (n1, length (getPairsOrderedByDistance xs))) $ \n2 ->
        let groups1 = part1Groups n1 xs
            groups2 = part1Groups n2 xs
            maxSize ys = if null ys then 0 else maximum (Set.size <$> ys)
        in maxSize groups1 <= maxSize groups2

prop_totalGroupSizeMonotone :: Property
prop_totalGroupSizeMonotone = forAll genV3List $ \xs ->
    forAll (choose (0, length (getPairsOrderedByDistance xs))) $ \n1 ->
    forAll (choose (n1, length (getPairsOrderedByDistance xs))) $ \n2 ->
        let total ys = sum (Set.size <$> ys)
        in total (part1Groups n1 xs) <= total (part1Groups n2 xs)

prop_fullGraphSingleComponent :: Property
prop_fullGraphSingleComponent = forAll genV3List $ \xs ->
    let n = length (getPairsOrderedByDistance xs)
        groups = part1Groups n xs
    in if length xs < 2
        then null groups
        else normalizeGroups groups == normalizeGroups [Set.fromList xs]

prop_groupsDisjoint :: Property
prop_groupsDisjoint = forAll genV3List $ \xs ->
    forAll (choose (0, length (getPairsOrderedByDistance xs))) $ \n ->
        let groups = part1Groups n xs
        in all (\[a,b] -> Set.intersection a b == Set.empty) $ chooseSets 2 groups

prop_groupsElementsFromInput :: Property
prop_groupsElementsFromInput = forAll genV3List $ \xs ->
    forAll (choose (0, length (getPairsOrderedByDistance xs))) $ \n ->
        let groups = part1Groups n xs
            elems = concatMap Set.toList groups
        in all (`elem` xs) elems

prop_uniqueLengthsConsistent :: Property
prop_uniqueLengthsConsistent = forAll genV3List $ \xs ->
    forAll (choose (0, length (getPairsOrderedByDistance xs))) $ \n ->
        let groups = part1Groups n xs
            lengths = fmap Set.size groups
            uniques = part1UniqueLengths n xs
        in all (`elem` lengths) uniques && length uniques == length (nub uniques)


main :: IO ()
main = defaultMain $ testGroup "AoC5"
    [ testCase "parser parses test_input" $ do
        input <- TIO.readFile "test_input"
        assertBool "expected parse to succeed" (isRight (parse parser "test_input" input))
    , testCase "parser parses input" $ do
        input <- TIO.readFile "input"
        assertBool "expected parse to succeed" (isRight (parse parser "input" input))
    , testCase "part1 example" $ do
        input <- TIO.readFile "test_input"
        case parse parser "test_input" input of
            Left err -> assertFailure (show err)
            Right parsed -> assertEqual "part1" 40 (part1Test parsed)
    , testCase "part1 num elems in groups and in pairs is the same" $ do
        input <- TIO.readFile "input"
        case parse parser "test_input" input of
            Left err -> assertFailure (show err)
            Right parsed -> assertEqual "part1" (sum (length <$> part1Groups 1000 parsed)) (length $ nub $ concatMap (\(a,b) -> [a,b] )$ take 1000 $ getPairsOrderedByDistance $ parsed)
    , testCase "part1 groups are disjoint" $ do
        input <- TIO.readFile "input"
        case parse parser "test_input" input of
            Left err -> assertFailure (show err)
            Right parsed -> assertBool "part1" $ all (\[a,b] -> Set.intersection a b == Set.empty) $ chooseSets 2 $ part1Groups 1000 parsed
    -- , testProperty "part1 pairs length matches combinations" prop_pairsLength
    -- , testProperty "part1 pairs sorted by distance" prop_pairsSortedByDistance
    -- , testProperty "part1 pairs only use input elements" prop_pairsElementsFromInput
    -- , testProperty "distanceInt symmetry" prop_distanceSymmetry
    -- , testProperty "distanceInt zero on identical points" prop_distanceZero
    -- , testProperty "pairs empty for <2 elements" prop_pairsEmptyWhenSmall
    -- , testProperty "part1 groups empty when n=0" prop_groupsEmptyWhenZero
    -- , testProperty "part1 groups total size bounded" prop_groupsTotalSizeBounded
    -- , testProperty "part1 unique lengths within bounds" prop_uniqueLengthsWithinBounds
    -- , testProperty "part1 permutation invariant with unique distances" prop_permutationInvariantWhenUniqueDistances
    -- , testProperty "part1 groups are connected by first n pairs" prop_groupsAreConnected
    -- , testProperty "part1 max group size monotone in n" prop_maxGroupSizeMonotone
    -- , testProperty "part1 total group size monotone in n" prop_totalGroupSizeMonotone
    -- , testProperty "part1 full graph yields single component" prop_fullGraphSingleComponent
    -- , testProperty "part1 groups are disjoint (qc)" prop_groupsDisjoint
    -- , testProperty "part1 group elements come from input" prop_groupsElementsFromInput
    -- , testProperty "part1 unique lengths are unique and from groups" prop_uniqueLengthsConsistent
    , testCase "part2 example" $ do
        input <- TIO.readFile "test_input"
        case parse parser "test_input" input of
            Left err -> assertFailure (show err)
            Right parsed -> assertEqual "part2" 25272 (part2 parsed)
    ]
