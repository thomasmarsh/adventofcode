module Main where

import System.Environment
import Data.List
import Data.List.Split

area :: [Int] -> Int
area dim = 2*l*w + 2*w*h + 2*h*l
    where [l,w,h] = dim

smallest :: [Int] -> [Int]
smallest dim = take 2 $ sort dim

paper :: [Int] -> Int
paper dim = area dim + product (smallest dim)

ribbon :: [Int] -> Int
ribbon dim = perim + bow
    where perim = sum $ map (*2) (smallest dim)
          bow = product dim

parseLine :: String -> [Int]
parseLine s = map (read::String->Int) $ splitOn "x" s

part1 :: [String] -> Int
part1 ss = sum . map paper $ map parseLine ss

part2 :: [String] -> Int
part2 ss = sum . map ribbon $ map parseLine ss

main :: IO ()
main = do
   args <- getArgs
   content <- readFile (head args)
   let l = lines content
   putStrLn $ "Part 1: " ++ show (part1 l)
   putStrLn $ "Part 1: " ++ show (part2 l)
