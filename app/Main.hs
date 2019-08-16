module Main where

import System.Environment (getArgs)

import Y2018.D16 (run)

main :: IO ()
main = do
    [path] <- getArgs
    run path
