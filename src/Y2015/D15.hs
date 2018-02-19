module Y2015.D15 where

import System.Environment (getArgs)
import Data.List (transpose, maximumBy)
import Data.Ord (comparing)

-- Parsing

parseLine :: [String] -> [Int]
parseLine [_, "capacity", a,
              "durability", b,
              "flavor", c, 
              "texture", d,
              "calories", e] = [x a, x b, x c, x d, read e::Int]
    where x s = read $ init s::Int
parseLine _ = error "parse error"

parse :: String -> [[Int]]
parse = map (parseLine . words) . lines

-- Scoring

distribute :: [[Int]] -> [Int] -> [[Int]]
distribute props measures = [map (* snd x) (fst x)
                                    | x <- zip props measures]

eval :: [[Int]] -> [Int] -> (Int, Int)
eval props measures = (score, calories)
    where d = distribute props measures
          calories = sum $ map last d
          score = product $ (map (relu . sum) . transpose) (map init d)
          relu n | n < 0 = 0 | otherwise = n 


-- Searching and Ranking

amounts :: [[Int]]
amounts = [[i,j,k,n-(i+j+k)] | i <- [0..n],
                               j <- [0..n-i],
                               k <- [0..n-i-j]]
    where n = 100

evalMany :: [[Int]] -> [[Int]] -> [(Int, Int)]
evalMany = map . eval

solution :: [[Int]] -> (Int, Int)
solution props = (fst $ high rs, fst $ high nutritional)
    where rs = evalMany props amounts
          high = maximumBy (comparing fst)
          nutritional = filter ((==500) . snd) rs

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let props = parse contents
    let (part1, part2) = solution props
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
