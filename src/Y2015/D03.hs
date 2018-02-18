module Y2015.D03 where

import System.Environment (getArgs)
import Data.List (scanl, sort, group)

move :: Char -> (Int, Int) -> (Int, Int)
move c (x,y)
    | c == '^' = (x,y+1)
    | c == 'v' = (x,y-1)
    | c == '<' = (x-1,y)
    | c == '>' = (x+1,y)
    | otherwise = (x,y)

houses :: String -> [(Int, Int)]
houses = scanl (flip move) (0, 0)

unique :: Ord a => [a] -> Int
unique = length . group . sort

each :: Int -> [a] -> [a]
each n = map head . takeWhile (not . null) . iterate (drop n)

everyOther :: [a] -> [a]
everyOther = each 2

deliver :: String -> Int
deliver = unique . houses

deliver2 :: String -> Int
deliver2 route = unique (santa ++ robot)
    where santa = houses $ everyOther route
          robot = houses $ everyOther (tail route)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (head args)
    putStrLn $ "Part 1: " ++ show (deliver content)
    putStrLn $ "Part 2: " ++ show (deliver2 content)
