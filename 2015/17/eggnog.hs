import System.Environment (getArgs)
import Data.List (minimumBy)
import Data.Ord (comparing)

target = 150

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [xs !! i : x | i <- [0..(length xs)-1],
                        x <- combinations (n-1) (drop (i+1) xs)]

solve :: [Int] -> (Int, Int)
solve xs = (length targetCapacity, length targetQuantity)
    where allCombos = concat [combinations n xs | n <- [1..length xs]]
          targetCapacity = filter (\x -> sum x == target) allCombos
          minimumLength = length $ minimumBy (comparing length) targetCapacity
          targetQuantity = filter (\x -> length x == minimumLength) targetCapacity

main = do
    [path] <- getArgs
    contents <- readFile path
    let containers = map (read::String->Int) (lines contents)
    let (part1, part2) = solve containers
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
