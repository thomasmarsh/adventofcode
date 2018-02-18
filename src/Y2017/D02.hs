module Y2017.D02 where

import System.Environment (getArgs)

-- part 1

processRow :: String -> Int
processRow s = head $ evenlyDivisible ns
    where ns = map (read::String->Int) (words s)

sumLines :: String -> Int
sumLines = sum . map processRow . lines

-- part 2

isEvenlyDivisible :: Integral a => a -> a -> Bool
isEvenlyDivisible a b
    | a > b = a `rem` b == 0
    | otherwise = b `rem` a == 0

quotient :: Integral a => a -> a -> a
quotient a b
    | a > b = a `quot` b
    | otherwise = b `quot` a

evenlyDivisible :: [Int] -> [Int]
evenlyDivisible [] = []
evenlyDivisible (x:xs) = [ x `quotient` y
                         | y <- xs
                         , isEvenlyDivisible x y ] ++ evenlyDivisible xs

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    print $ sumLines contents
