module Y2015.D17 where

import System.Environment (getArgs)
import Data.List (subsequences)

target :: Int
target = 150

solve :: [Int] -> (Int, Int)
solve xs = (length targetCapacity, length targetQuantity)
    where targetCapacity = filter ((== target) . sum) $ subsequences xs
          minLen = minimum $ map length targetCapacity
          targetQuantity = filter ((== minLen) . length) targetCapacity

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let containers = map (read::String->Int) (lines contents)
    let (part1, part2) = solve containers
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
