module Main where

import System.Environment (getArgs)

import Y2017.D13 (run)

main :: IO ()
main = do
    [path] <- getArgs
    run path
