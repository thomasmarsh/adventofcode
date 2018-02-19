module Y2017.D06 where

import System.Environment (getArgs)
import Data.List (elemIndex)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

dup :: Ord a => [a] -> Int
dup zs = dup' 0 zs Set.empty
  where dup' _ [] _ = -1
        dup' n (x:xs) s = if Set.member x s
                           then n
                           else dup' (n+1) xs (Set.insert x s)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs
replaceNth _ _ [] = error "empty list"

highest :: [Int] -> Int
highest xs = fromMaybe (error "not found") idx
    where idx = elemIndex (maximum xs) xs

distribute :: Int -> [Int] -> [Int]
distribute idx xs = go start amt xs'
    where amt = xs !! idx
          xs' = replaceNth idx 0 xs
          start = (idx + 1) `mod` length xs
          go i n zs
            | n == 0 = zs
            | otherwise = go ((i+1) `mod` length xs) (n-1) zs'
            where zs' = replaceNth i ((zs !! i)+1) zs

step :: [Int] -> [Int]
step xs = distribute (highest xs) xs

cycleCount :: [Int] -> Int
cycleCount xs = dup $ iterate step xs

cycleLength :: [Int] -> Int
cycleLength xs = b - a
    where ys = iterate step xs
          b = dup ys
          a = fromMaybe (error "not found") (elemIndex (ys !! b) ys)

parse :: String -> [Int]
parse s = map (read::String->Int) $ words s

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let banks = parse contents
    print $ cycleCount banks
    print $ cycleLength banks
