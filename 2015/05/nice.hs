import System.Environment
import Data.List

allTheSame xs = and $ map (== head xs) (tail xs)

pairs s = map (take 2) tails'
    where tails' = take ((length s)-1) (tails s)

hasDupe' s = any allTheSame (pairs s)

nice s = and [noBad, hasDupe, vowelComplete]
    where noBad = not $ any (\x -> isInfixOf x s) bad
          bad = ["ab", "cd", "pq", "xy"]
          hasDupe = any allTheSame (pairs s)
          vowelComplete = 2 < (length . filter (==True) $ map isVowel s)
          isVowel c = elem c "aeiou"

hasWrapping s = any isWrapping (tails s)
    where isWrapping s
            | length s < 3 = False
            | otherwise = s !! 0 == s !! 2

hasTwoPairs s
    | (length s) < 4 = False
    | any (== (head p)) $ tail (tail p) = True
    | otherwise = hasTwoPairs (tail s)
    where p = pairs s

nice2 s = and [hasWrapping s, hasTwoPairs s]

checkNice :: (String->Bool) -> [String] -> Int
checkNice f l = length $ filter (==True) nice'
    where nice' = map f l

main = do
    [path] <- getArgs
    content <- readFile path
    putStrLn $ "Part 1: " ++ show (checkNice nice (lines content))
    putStrLn $ "Part 2: " ++ show (checkNice nice2 (lines content))
