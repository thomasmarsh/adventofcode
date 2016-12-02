-- My first Haskell program
import Data.List.Split
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ (show (calcDistance line))
            main

data State  = State { d :: Int, x :: Int, y :: Int }

initialState :: State
initialState = State { d = 0, x = 0, y = 0 }

right :: Int -> Int
right d = (d+1) `mod` 4

left :: Int -> Int
left d
    | d == 0 = 3
    | otherwise = (d-1) `mod` 4

rightS :: State -> State
rightS s = State { d = right (d s), x = x s, y = y s }

leftS :: State -> State
leftS s = State { d = left (d s), x = x s, y = y s }

handleTurn :: State -> Char -> State
handleTurn state c
    | c == 'R' = rightS state
    | c == 'L' = leftS state
    | otherwise  = state

forward :: State -> Int -> State
forward s n
    | (d s) == 0 = State { d = d s, x = x s,       y = (y s) - n }
    | (d s) == 1 = State { d = d s, x = (x s) + n, y = y s }
    | (d s) == 2 = State { d = d s, x = x s,       y = (y s) + n }
    | (d s) == 3 = State { d = d s, x = (x s) - n, y = y s }
    | otherwise = s

handleInstruction :: State -> String -> State
handleInstruction state s = forward (handleTurn state (s !! 0)) (read (tail s) :: Int)

calcState :: String -> State
calcState line = foldl (\x y -> handleInstruction x y) initialState (splitOn ", " line)

distance :: State -> Int
distance s = (x s) + (y s)

calcDistance :: String -> Int
calcDistance s = distance $ calcState(s)
