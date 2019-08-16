module Y2018.D12 where

import Control.Arrow ((&&&))
import Data.List (foldl', group)
import Data.List.Split (divvy, splitOn)
import qualified Data.Map as M

type Rules = M.Map [Bool] Bool

type LSys = ([Bool], Int, Rules)

showL :: LSys -> String
showL (state, offset, _) =
    map (\x ->
             if x
                 then '#'
                 else '.')
        state

parse :: String -> LSys
parse s = (parseState initial, 0, M.fromList $ map parseRule rest)
  where
    (first, rest) = (head &&& drop 2) $ lines s
    [_, initial] = splitOn ": " first
    parseState = map ((==) '#')
    parseRule xs = (parseState from, to == "#")
      where
        [from, to] = splitOn " => " xs

step :: LSys -> LSys
step (state, offset, rules) = (drop initialEmpty out, offset', rules)
  where
    state' =
        divvy
            5
            1
            (False :
             False : False : False : state ++ [False, False, False, False])
    out = map (rules M.!) state'
    initialEmpty = length . head . group $ out
    offset' = offset + initialEmpty - 2

potSum :: LSys -> Int
potSum (state, offset, _) =
    sum .
    map (\(n, x) ->
             if x
                 then n
                 else 0) $
    (zip [offset ..] state)

part1 :: LSys -> Int
part1 lsys = potSum $ (iterate step lsys !! 20)

diffList :: Int -> [Int] -> [Int]
diffList prev [] = [-prev]
diffList prev (x:xs) = prev - x : diffList x xs

part2 :: LSys -> Int
part2 lsys = sum ns + (50000000000 - n) * (b - a)
  where
    n = 180
    ns = map potSum (take n $ iterate step lsys)
    [a, b] = foldl' (const . drop 1) ns (drop 2 ns)

run :: String -> IO ()
run path = do
    lsys <- parse <$> readFile path
    putStrLn $ "Part 1: " ++ (show $ part1 lsys)
    putStrLn $ "Part 2: " ++ (show $ part2 lsys)
