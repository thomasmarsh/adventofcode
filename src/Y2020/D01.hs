{-# LANGUAGE RankNTypes #-}
module Y2020.D01 where

import Data.List

-- O(n^2) solution; binary search would be better for O(n*log(n))

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

triples :: [a] -> [(a,a,a)]
triples xs = [(a,b,c) | (a:as) <- tails xs, (b:bs) <- tails as, c <- bs]

parseFile :: String -> [Int]
parseFile = map read . lines

process :: forall a. (a -> (Int, Int)) -> ([Int] -> [a]) -> [Int] -> Int
process f g = snd . head . filter (\(x,y) -> x == 2020) . map f . g

part1 :: [Int] -> Int
part1 = process (\(a,b) -> (a+b, a*b)) pairs

part2 :: [Int] -> Int
part2 = process (\(a,b,c) -> (a+b+c, a*b*c)) triples

run :: String -> IO ()
run path = do
    xs <- parseFile <$> readFile path
    putStrLn $ "Part 1: " ++ show (part1 xs)
    putStrLn $ "Part 2: " ++ show (part2 xs)
