import System.Environment (getArgs)
import Data.List (sort, transpose)
import Data.List.Split (chunksOf)

parse :: String -> [[Int]]
parse s = map (map (read :: String->Int) . words) (lines s)

isValid :: [Int] -> Bool
isValid xs@[_,_,_] = (a+b) > c
    where [a,b,c] = sort xs
isValid _ = error "invalid column count"

validCount :: [[Int]] -> Int
validCount = length . filter isValid

tValidCount :: [[Int]] -> Int
tValidCount xs = (length . filter isValid) (concatMap (chunksOf 3) (transpose xs))

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let xs = parse contents
    putStrLn $ "Part 1: " ++ show (validCount xs)
    putStrLn $ "Part 2: " ++ show (tValidCount xs)
