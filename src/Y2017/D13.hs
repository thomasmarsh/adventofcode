{-# LANGUAGE OverloadedStrings #-}
module Y2017.D13 where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text.Read (decimal)

data Direction = Up | Down deriving (Eq, Show)

type Range = Int
type Pos = Int
type Severity = Int
type Time = Int

data Scanner
    = Scanner
    { range :: Range
    , pos   :: Pos
    , dir   :: Direction
    } deriving (Eq, Show)

nilScanner :: Scanner
nilScanner
    = Scanner
    { range = 0
    , pos = 0
    , dir = Down
    }

initScanner :: Range -> Scanner
initScanner r = nilScanner { range = r }

stepScanner :: Scanner -> Scanner
stepScanner s
    | range s == 0 = s
    | dir s == Up   && pos s == 0           = s { pos = 1, dir = Down }
    | dir s == Down && pos s == range s - 1 = s { pos = (pos s) - 1, dir = Up }
    | dir s == Up = s { pos = (pos s) - 1 }
    | otherwise   = s { pos = (pos s) + 1 }

stepScanners :: [Scanner] -> [Scanner]
stepScanners = fmap stepScanner

cost :: Time -> [Scanner] -> Severity
cost n [] = 0
cost n (x:xs)
    | otherwise = severity + next
    where
        severity
            | pos x == 0 = n * (range x)
            | otherwise = 0
        next = cost (n+1) (stepScanners xs)

minimumDelay :: [Scanner] -> Time
minimumDelay = go 0
    where
        go n xs@(x:_)
                | cost 0 xs == 0 && pos x /= 0 = n
                | otherwise = go (n+1) (stepScanners xs)

-- 
-- Parsing
-- 

denseToSparse :: [(Int, Int)] -> [Range]
denseToSparse = go 0
    where
        go :: Int -> [(Int, Int)] -> [Range]
        go _ [] = []
        go n a@((i,r):xs)
            | n == i = r:go (n+1) xs
            | otherwise = 0:go (n+1) a

parseInt :: T.Text -> Int
parseInt s =
    case decimal s of
        Right (n, _) -> n
        Left _ -> 0
    
parseLine :: T.Text -> (Int, Int)
parseLine s = pair parsed
    where
        parsed = fmap parseInt (T.splitOn  ": " s)
        pair [a,b] = (a,b)
        pair _ = error "parse error"

parse :: T.Text -> [Scanner]
parse s = map initScanner $ denseToSparse xs
    where xs = fmap parseLine (T.lines s)

run :: String -> IO ()
run path = do
    s <- T.readFile path
    let xs = parse s
    putStrLn $ "Part 1: " ++ (show $ cost 0 xs)
    putStrLn $ "Part 2: " ++ (show $ minimumDelay xs)
