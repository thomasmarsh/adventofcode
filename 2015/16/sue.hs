module Main where

import           Data.List (intersect)
import           Data.Map ((!), Map)
import qualified Data.Map as M
import           System.Environment (getArgs)

type Profile = Map String Int
type Sues = [(Int, Profile)]

target :: Profile
target = M.fromList [
    ("children", 3), ("cats",        7),
    ("samoyeds", 2), ("pomeranians", 3),
    ("akitas",   0), ("vizslas",     0),
    ("goldfish", 5), ("trees",       3),
    ("cars",     2), ("perfumes",    1)]

parse :: String -> Sues
parse s = map match ws
    where ws = map (map strip . tail . words) (lines s)
          strip x | last x == ',' || last x == ':' = init x | otherwise = x
          buildEntry k v = (k, read v :: Int)
          match [sueStr, s1, n1, s2, n2, s3, n3] =
                (read sueStr :: Int,
                 M.fromList [buildEntry s1 n1,
                             buildEntry s2 n2,
                             buildEntry s3 n3])
          match _ = error "bad parse"

values :: [String] -> Profile -> [Int]
values ks x = [x!k | k <- ks]

matches :: Profile -> Profile -> Bool
matches a b = values ks a == values ks b
    where ks = M.keys a `intersect` M.keys b

rangeMatches :: Profile -> Profile -> Bool
rangeMatches a b = all test ks
    where ks = M.keys a `intersect` M.keys b
          test k 
            | elem k ["cats", "trees"] = a' > b'
            | elem k ["pomeranians", "goldfish"] = a' < b'
            | otherwise = a' == b'
            where (a',b') = (a!k, b!k)

search :: Sues -> (Profile -> Profile -> Bool) -> Int
search [] _ = error "not found"
search ((sue, profile):xs) fn
    | fn profile target = sue
    | otherwise = search xs fn

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let sues = parse contents
    putStrLn $ "Part 1: " ++ show (search sues matches)
    putStrLn $ "Part 2: " ++ show (search sues rangeMatches)
