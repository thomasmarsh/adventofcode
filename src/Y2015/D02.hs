module Y2015.D02 where

import System.Environment (getArgs)
import Data.List
import Data.List.Split (splitOn)
import Control.Monad (liftM2, ap)

area :: [Int] -> Int
area [l,w,h] = 2*l*w + 2*w*h + 2*h*l
area _ = error "invalid dimensions"

smallest :: [Int] -> [Int]
smallest = take 2 . sort

paper :: [Int] -> Int
paper = liftM2 (+) area (product . smallest)

ribbon :: [Int] -> Int
ribbon = ap ((+) . sum . map (2 *) . smallest) product
-- ^ The bow is the product and the rest is the perimter

parseLine :: String -> [Int]
parseLine = map (read::String->Int) . splitOn "x"

part1 :: [String] -> Int
part1 = sum . map (paper . parseLine)

part2 :: [String] -> Int
part2 = sum . map (ribbon . parseLine)

main :: IO ()
main = do
   args <- getArgs
   content <- readFile (head args)
   let l = lines content
   putStrLn $ "Part 1: " ++ show (part1 l)
   putStrLn $ "Part 1: " ++ show (part2 l)
