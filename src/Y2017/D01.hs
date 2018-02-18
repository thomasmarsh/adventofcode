{-# LANGUAGE OverloadedStrings #-}

module Y2017.D01 where

import Data.Char (digitToInt)
import System.Environment (getArgs)
import qualified Data.Text as T

strip :: String -> String
strip  = T.unpack . T.strip . T.pack

-- part 1

rotate :: String -> String
rotate [] = error "empty list"
rotate z@(x:xs)
    | needsRotate = rotate (xs ++ [x])
    | otherwise = z
    where
        needsRotate = x == last xs

sumDigits :: String -> Int
sumDigits [] = error "empty list"
sumDigits [_] = 0
sumDigits (x:xs)
    | x == head xs = digitToInt x + sumDigits xs
    | otherwise = sumDigits xs

count :: String -> Int
count = sumDigits . rotate . strip

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    print $ count contents

-- part 2

rotate' :: Int -> [a] -> [a]
rotate' _ [] = []
rotate' n xs = zipWith const (drop n (cycle xs)) xs

digitPairs :: String -> [(Char, Char)]
digitPairs xs = zip xs (rotate' (length xs `quot` 2) xs)

sumDigits' :: [(Char, Char)] -> Int
sumDigits' [] = 0
sumDigits' ((x,y):xs)
    | x == y = digitToInt x + sumDigits' xs
    | otherwise = sumDigits' xs

count' :: String -> Int
count' = sumDigits' . digitPairs . strip

main2 :: IO ()
main2 = do
    [path] <- getArgs
    contents <- readFile path
    print $ count' contents
