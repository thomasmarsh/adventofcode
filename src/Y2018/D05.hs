module Y2018.D05 where

import Data.Char (toLower, toUpper)

reduce :: String -> Int
reduce = length . go []
    where
        go xs []     = xs
        go [] (y:ys) = go [y] ys
        go (x:xs) (y:ys)
            | x /= y && toUpper x == toUpper y = go xs ys
            | otherwise                        = go (y:x:xs) ys

minReduction :: String -> Int
minReduction s = minimum ss
    where
        remove a b = a /= b && toUpper a /= b
        ss         = map (\x -> reduce . filter (remove x) $ s) ['a'..'z']

run :: String -> IO ()
run path = do
    contents <- readFile path
    let s = head $ words $ contents
    putStrLn $ "Part 1: " ++ show (reduce s)
    putStrLn $ "Part 2: " ++ show (minReduction s)
