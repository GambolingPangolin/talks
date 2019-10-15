{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Polysemy       (Embed, Member, Sem, embed, interpret, makeSem,
                                 run, runM)
import           Polysemy.Error (Error, runError, throw)
import           Polysemy.State (State, get, put, runState)
import           System.Random  (randomRIO)


data Game (m :: * -> *) a where
    Input :: Game m String
    Output :: String -> Game m ()
    RandomR :: Int -> Int -> Game m Int


makeSem ''Game


runGameIO :: Member (Embed IO) r => Sem (Game ': r) a -> Sem r a
runGameIO = interpret $ \case
    Input         -> embed   getLine
    Output x      -> embed $ putStrLn x
    RandomR lo hi -> embed $ randomRIO (lo, hi)


main :: IO ()
main = runM $ runGameIO game


game :: forall r . Member Game r => Sem r ()
game = do
    output "I'm thinking of a number between 1 and 100"
    randomR 1 100 >>= loop

    where

    loop :: Int -> Sem r ()
    loop n = do
        output "Guess a number"
        guess <- read <$> input
        if | guess == n -> output "You win!"
           | guess <  n -> output "Too low. Try again."   >> loop n
           | guess >  n -> output "Too high. Try again." >> loop n


--------------------------------------------------------------------------------
-- Testing

data World
    = World
    { inputs  :: [String]
    , outputs :: [String]
    } deriving (Eq, Show)


getInput
    :: forall r
     . ( Member (State World) r
       , Member (Error String) r
       )
    => Sem r String
getInput = get >>= go
    where
    go :: World -> Sem r String
    go (World (i:is) os) = i <$ put (World is os)
    go _                 = throw "EOF"


addOutput :: Member (State World) r => String -> Sem r ()
addOutput s = do
    World is os <- get
    put $ World is (s:os)


runGameTest
    :: ( Member (State World) r
       , Member (Error String) r
       )
    => Int -> Sem (Game ': r) a -> Sem r a
runGameTest n = interpret $ \case
    Input       -> getInput
    Output x    -> addOutput x

    RandomR lo hi | n < lo || n > hi -> throw "Bounds check"
                  | otherwise        -> return n


testGame :: [String] -> Int -> Either String [String]
testGame is n
    = fmap (outputs . fst)
    . run
    . runError
    . runState (World is [])
    . runGameTest n
    $ game
