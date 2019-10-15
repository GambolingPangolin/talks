{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           System.Random (randomRIO)

main :: IO ()
main = game


game :: IO ()
game = do
    putStrLn "I'm thinking of a number between 1 and 100"
    randomRIO @Int (1, 100) >>= loop

    where

    loop n = do
        putStrLn "Guess a number"
        guess <- read <$> getLine
        if | guess == n -> putStrLn "You win!"
           | guess <  n -> putStrLn "Too low. Try again."   >> loop n
           | guess >  n -> putStrLn "Too high. Try again." >> loop n
