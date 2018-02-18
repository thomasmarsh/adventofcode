module Y2016.D04 where

import Data.Char (chr, ord)
import Data.List (intercalate, nub, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

readSectorId :: String -> Int
readSectorId s = fromMaybe 0 (readMaybe s :: Maybe Int)

type Parsed = (String, Int, String)

parseLine :: String -> Parsed
parseLine room = (encrypted, sectorId, checksum)
    where parts = splitOn "[" room
          headParts = splitOn "-" $ head parts
          checksum = init $ last parts
          sectorId = readSectorId $ last headParts
          encrypted = intercalate "-" (init headParts)

rotate :: Int -> Char -> Char
rotate n c | c == '-' = ' '
           | otherwise = chr $ shifted + ord 'a'
     where normalized = ord c - ord 'a'
           shifted = (normalized + n) `mod` 26

decrypt :: Parsed -> String
decrypt (encrypted, sectorId, _) = map (rotate sectorId) encrypted

rc :: Parsed -> String
rc (encrypted, _, _) = take 5 $ map fst sorted
    where unique = nub $ filter (/= '-') encrypted
          counts = [length $ filter (== c) encrypted | c <- unique]
          sorted = sortBy cmp (zip unique counts)
          cmp (a, i) (b, j) | i /= j = compare j i
                            | otherwise = compare a b

sectorValue :: Parsed -> Int
sectorValue room@(_, sectorId, checksum)
    | rc room == checksum = sectorId
    | otherwise = 0

part1 :: [Parsed] -> Int
part1 ps = sum $ map sectorValue ps

part2 :: [Parsed] -> Int
part2 ps = sectorId
    where findNorthPole p = "northpole" == head (splitOn " " (decrypt p))
          (_, sectorId, _) = head $ filter findNorthPole ps

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let parsed = map parseLine (lines contents)
    putStrLn $ "Part 1: " ++ show (part1 parsed)
    putStrLn $ "Part 2: " ++ show (part2 parsed)
