{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

module Y2015.D06 where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Array.IO (IOUArray, MArray, newArray, readArray, writeArray)

bound :: Int
bound = 999

type Coord = (Int, Int)
type Lights = IOUArray Coord Int
type MLights = MArray IOUArray Int IO
data Mode = English | Elvish

parseCoord :: String -> Coord
parseCoord s = tuple $ map (read::String->Int) (splitOn "," s)
    where tuple [x, y] = (x, y)
          tuple _ = error "parse error"

parseLine :: String -> (Mode -> Int -> Int, Coord, Coord)
parseLine s = p ws
    where ws = words s
          p ["turn", "on", c1, "through", c2] = tuple on c1 c2
          p ["turn", "off", c1, "through", c2] = tuple off c1 c2
          p ["toggle", c1, "through", c2] = tuple toggle c1 c2
          p _ = error "parse error"
          tuple fn c1 c2 = (fn, parseCoord c1, parseCoord c2)

toggle :: Mode -> Int -> Int
toggle English n = 1-n
toggle Elvish n = n+2

on :: Mode -> Int -> Int
on English _ = 1
on Elvish n = n+1

off :: Mode -> Int -> Int
off English _ = 0 
off Elvish n
    | n > 0 = n-1
    | otherwise = 0

update :: MLights => Lights -> Mode -> (Mode -> Int -> Int, Coord, Coord) -> IO ()
update a m (fn, (x1, y1), (x2, y2)) = mapM_ update' [(x,y) | x <- [x1..x2], y <- [y1..y2]]
    where update' coord = do
            x <- readArray a coord
            writeArray a coord (fn m x)

apply :: MLights => Mode -> [(Mode -> Int -> Int, Coord, Coord)] -> IO Int
apply m xs = do
    lights <- newArray ((0, 0), (bound, bound)) 0
    mapM_ (update lights m) xs
    ts <- mapM (readArray lights) [(x, y) | x <- [0..bound], y <- [0..bound]]
    return $ sum ts

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let commands = map parseLine (lines contents)
    putStrLn =<< ("Part 1: " ++) . show <$> apply English commands
    putStrLn =<< ("Part 2: " ++) . show <$> apply Elvish commands
