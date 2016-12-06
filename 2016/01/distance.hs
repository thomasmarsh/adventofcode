-- My first Haskell program

import Data.List.Split

main = do
    line <- getLine
    putStrLn $ (show (calcDistance line))

right d | d >= 3 = 0 | otherwise = d + 1
left  d | d == 0 = 3 | otherwise = d - 1

handleTurn d c
    | c == 'R' = right d
    | c == 'L' = left d
    | otherwise = error "Unexpected direction"

forward pos d n
    | d == 0 = (x,   y-n)
    | d == 1 = (x+n, y)
    | d == 2 = (x,   y+n)
    | d == 3 = (x-n, y)
    | otherwise = (x, y)
    where
        x = fst pos
        y = snd pos

move (pos, dir) dirc n = (forward pos newDir n, newDir)
    where newDir = handleTurn dir dirc

handleInstruction state s = move state (s !! 0) (read (tail s) :: Int)

initialState = ((0, 0), 0)

calcState line = foldl handleInstruction initialState (splitOn ", " line)

distance s = (fst pos) + (snd pos)
    where pos = fst s

calcDistance s = distance $ calcState(s)
