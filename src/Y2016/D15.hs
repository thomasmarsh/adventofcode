module Y2016.D15 where

import System.Environment (getArgs)

type Config = [(Int, Int)]

parseWords :: [String] -> (Int, Int)
parseWords ["Disc", _, "has", n, "positions;",
            "at", "time=0,", "it", "is", "at", "position", p]
    = ((read::String->Int) n,
       (read::String->Int) (init p))
parseWords _ = error "parse error"

isGood :: Config -> Int -> Bool
isGood cfg t = and [pos == 0
                    | ((count, tzero), i) <- zip cfg [1..]
                    , let pos = (tzero + t + i) `mod` count]

search :: Config -> Int
search cfg = head [t | t <- [0..], isGood cfg t]

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let cfg = map (parseWords . words) (lines contents)
    putStrLn $ "Part 1: " ++ show (search cfg)
    putStrLn $ "Part 1: " ++ show (search (cfg ++ [(11,0)]))
