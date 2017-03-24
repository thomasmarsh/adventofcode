module Main where

import Data.List (group, sort, elemIndex)
import Data.Maybe (fromJust)

isqrt :: Int -> Int
isqrt = floor . (sqrt::Double->Double) . fromIntegral

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

factors :: Int -> [Int]
factors n = (rmdups . concat) xs
    where xs = [[i, n `quot` i] | i <- [1,1+step..isqrt n], n `mod` i == 0]
          step | n `mod` 2 == 1 = 2 | otherwise = 1

search :: Int -> (Int, Int)
search n = (p1, p2)
    where xs = [1..]
          d1 = map factors xs
          p1 = indexTrue $ map ((>= n) .  (*10) . sum) d1
          d2 = [filter ((<= 50) . quot i) d | (i,d) <- zip xs d1]
          p2 = indexTrue $ map ((>= n) . (*11) . sum) d2
          indexTrue ys = 1 + (fromJust $ True `elemIndex` ys)

main :: IO ()
main = do
    let (part1, part2) = search 34000000
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
