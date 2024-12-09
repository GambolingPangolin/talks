#!/usr/bin/env cabal
{- cabal:
ghc-options: -Wall
build-depends:
  base,
  bytestring,
  containers,
  optparse-applicative,
  random,
  text,
  time,
  transformers,
  vector,
-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main (main) where

import Control.Applicative ((<**>))
import Control.Arrow (first, second)
import Control.Monad (filterM, replicateM, when)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.State.Strict qualified as St
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as MV
import Options.Applicative qualified as Opt
import System.IO (IOMode (..), withFile)
import System.Random (StdGen)
import System.Random qualified as R
import System.Random.Stateful (IOGenM)
import System.Random.Stateful qualified as RS

-- | Natural persistent implementation
persistentQuicksortA :: (Ord a) => Vector a -> Vector a
persistentQuicksortA v
    | V.length v < 2 = v
    | V.length v == 2 =
        if v V.! 0 > v V.! 1 then swapV 0 1 v else v
    | otherwise =
        let
            v0 = v V.! 0
            vLeft = V.filter (<= v0) $ V.drop 1 v
            vRight = V.filter (> v0) v
         in
            persistentQuicksortB vLeft <> V.singleton v0 <> persistentQuicksortB vRight

-- | Generic in-place implementation
quicksort ::
    (Monad m, Ord a) =>
    (Int -> m a) ->
    (Int -> Int -> m ()) ->
    Int ->
    m ()
quicksort getIX swap n
    | n < 2 = pure ()
    | otherwise = start 0 (n - 1)
  where
    start ixLow ixHigh
        -- degenerate range
        | ixHigh <= ixLow = pure ()
        -- sort a 2 wide range directly
        | ixHigh == succ ixLow = do
            a <- getIX ixLow
            b <- getIX ixHigh
            when (a > b) $ swap ixLow ixHigh
        -- set up quicksort recursion
        | otherwise = inner ixLow (succ ixLow) ixHigh ixHigh
    inner ixLow cursorLow cursorHigh ixHigh
        -- It may be possible to move a large element to the "back" of the range
        | cursorLow < cursorHigh = do
            pivot <- getIX ixLow
            atCursorLow <- getIX cursorLow
            case compare atCursorLow pivot of
                GT ->
                    -- swap with top of range repeat analysis with narrower range at the top
                    swap cursorLow cursorHigh
                        >> inner ixLow cursorLow (pred cursorHigh) ixHigh
                _ ->
                    -- leave in place with narrower range at the bottom
                    inner ixLow (succ cursorLow) cursorHigh ixHigh
        -- no further swaps possible
        | otherwise = do
            pivot <- getIX ixLow
            atCursor <- getIX cursorLow
            let ixPivot =
                    if atCursor <= pivot
                        then cursorLow
                        else pred cursorLow
            -- Move the pivot to the middle of the range
            swap ixLow ixPivot
                -- Sort the front half
                >> start ixLow (pred ixPivot)
                -- Sort the back half
                >> start (succ ixPivot) ixHigh

-- | Specialize quicksort to modify a vector using persistent updates via the state monad
persistentQuicksortB :: (Ord a) => Vector a -> Vector a
persistentQuicksortB vInput = St.execState (quicksort getIX swap n) vInput
  where
    n = V.length vInput
    getIX ix = St.gets (V.! ix)
    swap i j = St.modify $ swapV i j

swapV :: Int -> Int -> Vector a -> Vector a
swapV i j v = v V.// [(i, v V.! j), (j, v V.! i)]

-- | Specialize quicksort to update a mutable vector in place using ST
mutableQuicksort :: (Ord a) => Vector a -> Vector a
mutableQuicksort vInput = runST $ do
    vMut <- V.thaw vInput
    quicksort (MV.read vMut) (MV.swap vMut) (MV.length vMut)
    V.freeze vMut

data Graph = Graph
    { nVertices :: !Int
    , neighbors :: !(IntMap [Int])
    }
    deriving (Eq, Show)

type VertexState = (Maybe [Int], Bool)

-- | Generic Dijkstra implementation
dijkstra ::
    (Monad m) =>
    (Int -> m VertexState) ->
    (Int -> (VertexState -> VertexState) -> m ()) ->
    m (Maybe Int) ->
    Graph ->
    Int ->
    Int ->
    m (Maybe [Int])
dijkstra getState updateState nextToVisit graph vStart vEnd = visit vStart
  where
    visit v
        | v == vEnd = fmap (reverse . (v :)) <$> getPath v
        | otherwise = do
            ns <- unvisitedNeighbors v
            path <- getPath v
            mapM_ (update $ (v :) <$> path) ns
            setVisited v
            nextToVisit >>= maybe (pure Nothing) visit
    update thisPath n = do
        getPath n >>= \case
            Just existingPath | length thisPath < length existingPath -> setPath n thisPath
            _ -> setPath n thisPath
    unvisitedNeighbors v =
        filterM isUnvisitedM $ IM.findWithDefault mempty v (neighbors graph)

    isUnvisitedM = fmap (not . snd) . getState
    getPath = fmap fst . getState

    setVisited v = updateState v . second $ const True
    setPath n = updateState n . first . const

-- | Specialization using state monad
persistentDijkstra :: (Graph, Int, Int) -> Maybe [Int]
persistentDijkstra (graph, vStart, vEnd) =
    St.evalState
        (dijkstra getState updateState nextToVisit graph vStart vEnd)
        v0
  where
    v0 =
        V.replicate (nVertices graph) (Nothing, False)
            V.// [(vStart, (Just [], True))]
    getState = St.gets . flip (V.!)
    updateState i f = St.modify $ \v ->
        let x = v V.! i
         in v V.// [(i, f x)]
    nextToVisit = St.gets $ fmap fst . V.ifoldl' nextToVisitStep Nothing

-- | Specialization using ST monad
mutableDijkstra :: (Graph, Int, Int) -> Maybe [Int]
mutableDijkstra (graph, vStart, vEnd) = runST $ do
    s <- MV.replicate (nVertices graph) (Nothing, False)
    MV.write s vStart (Just [], True)
    let getState = MV.read s
        updateState = flip $ MV.modify s
    dijkstra getState updateState (nextToVisit s) graph vStart vEnd
  where
    nextToVisit :: MVector s VertexState -> ST s (Maybe Int)
    nextToVisit = (fmap . fmap) fst . MV.ifoldl' nextToVisitStep Nothing

nextToVisitStep :: Maybe (Int, Int) -> Int -> VertexState -> Maybe (Int, Int)
nextToVisitStep =
    \cases
        (Just (_, n)) ix (Just path, False)
            | n > length path -> Just (ix, length path)
        Nothing ix (Just path, False) -> Just (ix, length path)
        accum _ _ -> accum

data ExampleName = Quicksort | Dijkstra

type Variant = Text

getExample :: IO (Variant, ExampleName)
getExample =
    Opt.execParser
        . Opt.info (opts <**> Opt.helper)
        $ Opt.progDesc "ST monad examples"
  where
    opts = (,) <$> optApproach <*> optExampleName
    optApproach = Opt.strOption $ Opt.long "variant"
    optExampleName =
        Opt.hsubparser $
            mconcat
                [ Opt.command "quicksort"
                    . Opt.info (pure Quicksort)
                    $ Opt.progDesc "Quicksort"
                , Opt.command "dijkstra"
                    . Opt.info (pure Dijkstra)
                    $ Opt.progDesc "Dijkstra"
                ]

main :: IO ()
main = do
    g <- RS.newIOGenM =<< R.initStdGen
    getExample
        >>= ( \case
                (variant, Quicksort) ->
                    runExamples g nExamples $
                        Example
                            (randomVector 1000)
                            ( case variant of
                                "persistent-a" -> persistentQuicksortA
                                "persistent-b" -> persistentQuicksortB
                                "mutable" -> mutableQuicksort
                                _ -> unknownVariant
                            )
                            defaultMarshal
                (variant, Dijkstra) -> do
                    runExamples g nExamples $
                        Example
                            (randomDijkstra 500 0.05)
                            ( case variant of
                                "persistent" -> persistentDijkstra
                                "mutable" -> mutableDijkstra
                                _ -> unknownVariant
                            )
                            defaultMarshal
            )
        >>= printResult
  where
    nExamples = 600
    unknownVariant = error "Unknown variant"

data Example i o = Example
    { getRandomInput :: IOGenM StdGen -> IO i
    , getOutput :: i -> o
    , marshal :: i -> o -> ByteString
    }

data ExampleResult = ExampleResult
    { nCases :: !Int
    , meanTime :: !NominalDiffTime
    }

runExamples ::
    IOGenM StdGen ->
    -- | Number of examples
    Int ->
    Example i o ->
    IO ExampleResult
runExamples g n example = withFile "st-talk.out" WriteMode $ \h ->
    mkResult
        <$> replicateM n (runExample h)
  where
    runExample h = do
        start <- Time.getCurrentTime
        input <- getRandomInput example g
        BS.hPut h . marshal example input $ getOutput example input
        BS.hPut h "\n"
        end <- Time.getCurrentTime
        pure $ end `Time.diffUTCTime` start
    mkResult xs = ExampleResult n . meanFinal $ L.foldl' meanAccum (0, 0 :: Int) xs
    meanAccum (s, c) x = (s + x, c + 1)
    meanFinal = \case
        (_, 0) -> 0
        (s, c) -> s / fromIntegral c

printResult :: ExampleResult -> IO ()
printResult ExampleResult{nCases, meanTime} =
    putStrLn $ show nCases <> " with mean time " <> show meanTime

randomVector :: Int -> IOGenM StdGen -> IO (Vector Int)
randomVector n g = V.fromList <$> replicateM n (RS.randomRM (-1000, 1000) g)

-- | Assume n > 1
randomDijkstra :: Int -> Double -> IOGenM StdGen -> IO (Graph, Int, Int)
randomDijkstra n p g = do
    s <- RS.randomRM (0, pred n) g
    t <- RS.randomRM (if s == 0 then (1, pred n) else (0, pred s)) g
    graph <-
        Graph n
            . IM.fromListWith (<>)
            . catMaybes
            <$> sequenceA
                [ do
                    edgeP <- RS.randomRM (0, 1) g
                    pure $ if edgeP < p then Just (i, [j]) else Nothing
                | i <- [0 .. pred n]
                , j <- [0 .. pred n]
                , i /= j
                ]
    pure (graph, s, t)

defaultMarshal :: (Show i, Show o) => i -> o -> ByteString
defaultMarshal input output = BS8.unlines $ BS8.pack <$> [show input, show output]
