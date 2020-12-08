module Y2020.D06 where

import Data.List (foldl', intersect, nub, sort)
import Data.List.Split (splitOn)
import qualified Data.Set as S

parse :: String -> [[String]]
parse = splitOn [""] . lines

part1 :: [[String]] -> Int
part1 = sum . map (length . nub . sort . mconcat)

common :: [String] -> String
common = nub . go . sort
    where go (x:xs) = foldl' intersect x xs

part2 = sum . map (length . common)

run :: String -> IO ()
run path = do
    ps <- parse <$> readFile path
    putStrLn $ "Part 1: " ++ show (part1 ps)
    putStrLn $ "Part 2: " ++ show (part2 ps)
