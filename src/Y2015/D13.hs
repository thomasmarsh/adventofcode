module Y2015.D13 where

import           Data.List (maximumBy, nub, permutations)
import           Data.List.Split (divvy)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)
import           System.Environment (getArgs)

type Weights = M.Map (String, String) Int

parseLine :: [String] -> ((String, String), Int)
parseLine [who, "would", verb, n, "happiness", "units", "by",
           "sitting", "next", "to", other] =
    ((who, init other), gainLose verb (read n))
    where
        gainLose "lose" m = -m
        gainLose "gain" m = m
        gainLose _ _ = error "bad verb"
parseLine _ = error "bad preference"

parse :: String -> Weights
parse s = M.fromList entries
    where entries = map (parseLine . words) (lines s)

guests :: Weights -> [String]
guests w = nub [a | (a, _) <- M.keys w]

triples :: [a] -> [[a]]
triples = divvy 3 1 . wrap
    where wrap [] = []
          wrap [x] = [x]
          wrap [x,y] = [x,y,x]
          wrap z@(x:y:_) = z ++ [x,y]

uniquePerms :: Weights -> [[String]]
uniquePerms w = map (head g :) $ permutations (tail g)
    where g = guests w

happiness :: Weights -> [String] -> ([String], Int)
happiness w g = (g, sum $ map happiness' (triples g))
    where happiness' triple
            | length triple == 3 = look l + look r
            | otherwise = error "arity mismatch"
              where [l,p,r] = triple
                    look k = fromJust (M.lookup (p, k) w)

happiest :: Weights -> Int
happiest w = snd (maximumBy (comparing snd) s)
    where s = map (happiness w) (uniquePerms w)

addSelf :: Weights -> Weights
addSelf w = add' w (guests w)
    where add' y [] = y
          add' y (x:xs) = M.insert ("self", x) 0 (
                            M.insert (x, "self") 0 (
                                add' y xs))

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let w = parse contents
    putStrLn $ "Part 1: " ++ show (happiest w)
    putStrLn $ "Part 1: " ++ show (happiest (addSelf w))

