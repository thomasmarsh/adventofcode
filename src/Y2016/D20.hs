module Y2016.D20 where

import System.Environment (getArgs)
import Data.List (sort)
import Data.List.Split (splitOn)

fixCount :: Int -> Int -> Int
fixCount count n = count + (2::Int)^(32::Int) - n

solve :: [(Int, Int)] -> (Int, Int)
solve bs = go bs 0 0 (maxBound :: Int)
    where go [] n count lowest = (lowest, fixCount count n)
          go ((lo, hi):xs) n count lowest = go xs n' count' lowest'
            where n'      = if   n <= hi
                            then hi + 1
                            else n

                  lowest' = if   n < lo && lowest == (maxBound :: Int)
                            then n
                            else lowest

                  count'  = if   n < lo
                            then count + lo - n
                            else count

parse :: String -> (Int, Int)
parse s = (read a :: Int, read b :: Int)
    where [a, b] = splitOn "-" s

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let bs = sort $ map parse (lines contents)
    let (part1, part2) = solve bs
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
