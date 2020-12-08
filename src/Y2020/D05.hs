module Y2020.D05 where

import Control.Arrow ((&&&))
import Data.Digits (unDigits)
import Data.List (sort)

parseRowCol :: String -> (Int, Int)
parseRowCol = rowDigits &&& colDigits
    where
        parse one s = unDigits 2 $ (\c -> if c == one then 1 else 0) <$> s
        rowDigits = parse 'B' . take 7
        colDigits = parse 'R' . drop 7

seatId :: (Int, Int) -> Int
seatId (row,col) = row*8+col

parse :: String -> [Int]
parse = map (seatId . parseRowCol) . lines

search :: [Int] -> Int
search (a:b:xs)
  | b - a == 2 = a + 1
  | otherwise = search (b:xs)

part1,part2 :: [Int] -> Int
part1 = maximum
part2 = search . sort

run :: String -> IO ()
run path = do
    ps <- parse <$> readFile path
    putStrLn $ "Part 1: " ++ show (part1 ps)
    putStrLn $ "Part 2: " ++ show (part2 ps)
