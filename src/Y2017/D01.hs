module Y2017.D01 where

{-# LANGUAGE OverloadedStrings #-}

import Data.Char (digitToInt)
import System.Environment (getArgs)
import qualified Data.Text as T

strip  = T.unpack . T.strip . T.pack

-- part 1

rotate :: String -> String
rotate z@(x:xs)
    | needsRotate = rotate (xs ++ [x])
    | otherwise = z
    where
        needsRotate = x == last xs

sumDigits [] = error "empty list"
sumDigits [x] = 0
sumDigits (x:xs)
    | x == head xs = (digitToInt x) + sumDigits xs
    | otherwise = sumDigits xs

count = sumDigits . rotate . strip

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    putStrLn $ show $ count contents

-- part 2

rotate' :: Int -> [a] -> [a]
rotate' _ [] = []
rotate' n xs = zipWith const (drop n (cycle xs)) xs

digitPairs :: String -> [(Char, Char)]
digitPairs xs = zip xs (rotate' (length xs `quot` 2) xs)

sumDigits' :: [(Char, Char)] -> Int
sumDigits' [] = 0
sumDigits' ((x,y):xs)
    | x == y = (digitToInt x) + sumDigits' xs
    | otherwise = sumDigits' xs

count' = sumDigits' . digitPairs . strip

main2 :: IO ()
main2 = do
    [path] <- getArgs
    contents <- readFile path
    putStrLn $ show $ count' contents
