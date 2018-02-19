module Y2017.D04 where

import System.Environment (getArgs)
import Data.List (nub, sort)

valid :: String -> Bool
valid s = length w1 == length w2
    where w1 = words s
          w2 = nub w1

isAnagramOf :: String -> String -> Bool
isAnagramOf a b = sort a == sort b

containsAnagram :: String -> [String] -> Bool
containsAnagram a ws = 0 /= length (filter (isAnagramOf a) ws)

isAnagram :: [String] -> Bool
isAnagram ss = not $ or $ go ss
    where go [_] = [False]
          go (a:ws) = containsAnagram a ws : go ws
          go [] = error "empty"

valid2 :: String -> Bool
valid2 s = isAnagram $ words s

validCount :: (String -> Bool) -> [String] -> Int
validCount fn ss = length $ filter id (map fn ss)

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let ls = lines contents
    print $ validCount valid ls
    print $ validCount valid2 ls
