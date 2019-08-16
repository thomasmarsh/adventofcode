module Y2015.D01 where

import Data.List
import Data.Maybe
import System.Environment

trans :: Char -> Int
trans c
    | c == '(' = 1
    | c == ')' = -1
    | otherwise = 0

part1 :: String -> Int
part1 = sum . map trans

part2 :: String -> Int
part2 s = 1 + (fromJust . elemIndex (-1) . scanl1 (+) $ map trans s)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (head args)
    putStrLn $ "Part 1: " ++ show (part1 content)
    putStrLn $ "Part 2: " ++ show (part2 content)
