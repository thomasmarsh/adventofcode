module Y2016.D01 where

import           Data.Char (isSpace)
import           Data.List.Split (splitOn)
import           Data.List (dropWhileEnd, dropWhile)
import qualified Data.Set as S
import           System.Environment (getArgs)

data Direction = North | East | South | West
    deriving (Bounded, Eq, Show, Enum)

type Pos = (Int, Int)
type State = (Pos, Direction)
type Locations = S.Set Pos

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

right :: Direction -> Direction
right d | d == West = North | otherwise = succ d

left :: Direction -> Direction
left d | d == North = West | otherwise = pred d

handleTurn :: Direction -> Char -> Direction
handleTurn d c
    | c == 'R' = right d
    | c == 'L' = left d
    | otherwise = error "Unexpected direction"

forward :: Int -> State -> State
forward n (pos, d)
    | d == North = ((x,   y-n), d)
    | d == East  = ((x+n, y), d)
    | d == South = ((x,   y+n), d)
    | d == West  = ((x-n, y), d)
    | otherwise  = ((x, y), d)
    where
        x = fst pos
        y = snd pos

move :: State -> Char -> Int -> State
move (pos, dir) dirc n = forward n (pos, newDir)
    where newDir = handleTurn dir dirc

handleInstruction :: State -> String -> State
handleInstruction state s = move state (s !! 0) (read (tail s) :: Int)

pointsBetween :: Pos -> Pos -> [Pos]
pointsBetween a@(x1, y1) (x2, y2) = take n $ iterate fwd a
    where n | x1 /= x2 && y1 /= y2 = error "need cardinal dirction"
            | otherwise = max (abs (x1 - x2)) (abs (y1 - y2))
          dir | y1 > y2 = North | x1 < x2 = East
              | y1 < y2 = South | x1 > x2 = West
              | otherwise = error "no direction"
          fwd (x, y) = fst $ forward 1 ((x, y), dir)

initialState :: State
initialState = ((0, 0), North)

calcState :: String -> State
calcState line = foldl handleInstruction initialState (splitOn ", " line)

distance :: State -> Int
distance s = (fst pos) + (snd pos)
    where pos = fst s

calcDistance :: String -> Int
calcDistance s = distance $ calcState(s)

main :: IO ()
main = do
    [path] <- getArgs
    s <- readFile path
    putStrLn $ "Part 1: " ++ show (calcDistance s)

