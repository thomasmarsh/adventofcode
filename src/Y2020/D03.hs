module Y2020.D03 where

import Data.List.Split (splitOn)
import Debug.Trace (trace)

data Space = Open | Tree deriving (Eq, Show)

parseSpace :: Char -> Space
parseSpace '.' = Open
parseSpace '#' = Tree

parseLine :: String -> [Space]
parseLine = map parseSpace

parse :: String -> [[Space]]
parse = map parseLine . lines

count :: (Int, Int) -> [[Space]] -> Int
count (dx, dy) = go 0 (0, 0)
  where
    go :: Int -> (Int, Int) -> [[Space]] -> Int
    go n (x,y) rows
      | length rows <= dy = n
      | otherwise =
          let
            (x', y') = (x+dx, y+dy)
            rows' = drop dy rows
            space = cycle (head rows') !! x'
            n' = n + if space == Tree then 1 else 0
           in go n' (x', y') rows'

part1 :: [[Space]] -> Int
part1 = count (3,1)

part2 :: [[Space]] -> Int
part2 rows = product $ map (`count`rows) slopes
    where
      slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

run :: String -> IO ()
run path = do
    ps <- parse <$> readFile path
    putStrLn $ "Part 1: " ++ show (part1 ps)
    putStrLn $ "Part 2: " ++ show (part2 ps)
