module Y2015.D05 where

import System.Environment
import Data.List

allTheSame :: Eq a => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

pairs :: [a] -> [[a]]
pairs s = map (take 2) tails'
    where tails' = take (length s-1) (tails s)

nice :: String -> Bool
nice s = noBad && hasDupe && vowelComplete
    where noBad = not $ any (\x -> x `isInfixOf` s) bad
          bad = ["ab", "cd", "pq", "xy"]
          hasDupe = any allTheSame (pairs s)
          vowelComplete = 2 < (length . filter (==True) $ map isVowel s)
          isVowel c = c `elem` "aeiou"

hasWrapping :: String -> Bool
hasWrapping s = any isWrapping (tails s)
    where isWrapping s'
            | length s' < 3 = False
            | otherwise = head s' == s' !! 2

hasTwoPairs :: String -> Bool
hasTwoPairs s
    | length s < 4 = False
    | any (== head p) $ tail (tail p) = True
    | otherwise = hasTwoPairs (tail s)
    where p = pairs s

nice2 :: String -> Bool
nice2 s = hasWrapping s && hasTwoPairs s

checkNice :: (String->Bool) -> [String] -> Int
checkNice f l = length $ filter (==True) nice'
    where nice' = map f l

main :: IO ()
main = do
    [path] <- getArgs
    content <- readFile path
    putStrLn $ "Part 1: " ++ show (checkNice nice (lines content))
    putStrLn $ "Part 2: " ++ show (checkNice nice2 (lines content))
