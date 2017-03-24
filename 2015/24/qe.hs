module Main where

import System.Environment (getArgs)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [xs !! i : x | i <- [0..(length xs)-1]
                                 , x <- combinations (n-1) (drop (i+1) xs)]

solve :: [Integer] -> Integer -> Integer
solve d n = solve' 1 
    where target = (sum d) `quot` n
          solve' i
            | i == length d = -1
            | not (null q) = minimum q
            | otherwise = solve' (i+1)
            where combos = combinations i d
                  filtered = filter ((== target) . sum) combos
                  q = map (foldl1 (*)) filtered

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let xs = map (read :: String -> Integer) (lines contents)
    putStrLn $ "Part 1: " ++ show (solve xs 3)
    putStrLn $ "Part 2: " ++ show (solve xs 4)
