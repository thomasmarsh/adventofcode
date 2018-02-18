module Y2015.D08 where

import System.Environment (getArgs)
import Data.Char (isHexDigit)

unescaped :: Int -> String -> Int
unescaped n [] = n
unescaped n ('\\':'\\':xs) = unescaped (n+1) xs
unescaped n ('\\':'"':xs) = unescaped (n+1) xs
unescaped n ('\\':'x':a:b:xs) 
    | isHexDigit a && isHexDigit b = unescaped (n+1) xs
    | otherwise = error "parse error"
unescaped n (_:xs) = unescaped (n+1) xs

escaped :: Int -> String -> Int
escaped n [] = n
escaped n ('"':xs) = escaped (n+2) xs
escaped n ('\\':xs) = escaped (n+2) xs
escaped n (_:xs) = escaped (n+1) xs

p1 :: String -> Int
p1 s = length s - unescaped (-2) s

p2 :: String -> Int
p2 s = escaped 2 s - length s

size :: (String -> Int) -> [String] -> Int
size fn ss = sum $ map fn ss

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let ss = lines contents
    putStrLn $ "Part 1: " ++ show (size p1 ss)
    putStrLn $ "Part 2: " ++ show (size p2 ss)
