module Y2020.D02 where

import Data.List.Split (splitOn)

data Policy = P Int Int Char deriving Show

conforms1 :: Policy -> String -> Bool
conforms1 (P a b c)
  = (\x -> x >= a && x <= b)
  . length
  . filter (==c)

conforms2 :: Policy -> String -> Bool
conforms2 (P a b c) s =
  let
    (x,y) = (s !! (a - 1), s !! (b - 1))
  in
    (x == c && y /= c) || (x /= c && y == c)

parseLine :: String -> (Policy, String)
parseLine s = (P a b (head r),  pw)
  where
    [x, pw] = splitOn ": " s
    [l, r] = splitOn " " x
    [a, b] = read <$> splitOn "-" l

parse :: String -> [(Policy, String)]
parse = map parseLine . lines

eval :: (Policy -> String -> Bool) -> [(Policy, String)] -> Int
eval f = length . filter (uncurry f)

part1,part2 :: [(Policy, String)] -> Int
part1 = eval conforms1
part2 = eval conforms2

run :: String -> IO ()
run path = do
    ps <- parse <$> readFile path
    putStrLn $ "Part 1: " ++ show (part1 ps)
    putStrLn $ "Part 2: " ++ show (part2 ps)
