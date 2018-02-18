module Y2016.D16 where

import Data.List.Split (chunksOf)

invert :: Char -> Char
invert '1' = '0'
invert '0' = '1'
invert _ = error "bad input"

dragon :: String -> Int -> String
dragon s n
    | length s >= n = s
    | otherwise = dragon (step s) n
    where step a = a ++ ['0'] ++ (map invert . reverse) a

checksum :: String -> String
checksum s
    | length s `mod` 2 /= 0 = s
    | otherwise = checksum $ (concatMap tr . chunksOf 2) s
    where tr [a,b] | a == b = "1" | otherwise = "0"
          tr _ = error "bad input"

search :: String -> Int -> String
search s n = checksum (take n (dragon s n))

main :: IO ()
main = do
    let input = "10111011111001111"
    let t1 = 272
    let t2 = 35651584
    putStrLn $ "Part 1: " ++ search input t1
    putStrLn $ "Part 2: " ++ search input t2
