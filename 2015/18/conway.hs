module Main where

import           System.Environment (getArgs)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.List (intercalate)

type Coord = (Int, Int)
type Lights = M.Map Coord Bool

neighbors :: Coord -> Coord -> [Coord]
neighbors dim (x, y) = valid dim [(x-1, y-1), (x, y-1), (x+1, y-1),
                                  (x-1, y),             (x+1, y),
                                  (x-1, y+1), (x, y+1), (x+1, y+1)]

valid :: Coord -> [Coord] -> [Coord]
valid (mx, my) = filter validCoord
    where validCoord (x, y)
            | x >= 0 && y >= 0 && x < mx && y < my = True
            | otherwise = False

isOn :: Lights -> Coord -> Bool
isOn lights (x, y) = fromJust $ M.lookup (x, y) lights

countOn :: Lights -> [Coord] -> Int
countOn lights = length . filter (isOn lights)

repr :: Lights -> Coord -> String
repr lights (mx, my) = intercalate "\n" [[chr $ isOn lights (x,y) | x <- [0..mx-1]] | y <- [0..my-1]]
    where chr True = '#'
          chr False = '.'

next :: Lights -> Coord -> Coord -> Bool
next lights dim t
    | on && (count == 2 || count == 3) = True
    | on = False
    | count == 3 = True
    | otherwise = False
    where count = countOn lights (neighbors dim t)
          on = isOn lights t

parseLight :: Char -> Bool
parseLight t
    | t == '#' = True
    | t == '.' = False
    | otherwise = error "parse error"

indices :: Coord -> [Coord]
indices (mx, my) = [(x, y) | y <- [0..my-1], x <- [0..mx-1]]

parseLights :: String -> (Lights, (Int, Int))
parseLights s = (lights, (mx, my))
    where lines' = lines s
          my = length lines'
          mx = length (head lines')
          lights = M.fromList $ zip (indices (mx, my)) (map parseLight (concat lines'))

fixCorners :: Coord -> Lights -> Lights
fixCorners (mx, my) lights = foldl (\t x -> M.insert x True t) lights corners
    where corners = [(0, 0),    (mx-1, 0),
                     (0, my-1), (mx-1, my-1)]

idFn :: Coord -> Lights -> Lights
idFn _ lights = lights

step :: (Coord -> Lights -> Lights) -> Coord -> Lights -> Lights
step fn dim lights =
        M.fromList [((x, y), next (fn dim lights) dim (x,y)) | (x, y) <- indices dim]

run :: Int -> (Coord -> Lights -> Lights) -> Coord -> Lights -> Int
run n fn dim lights = countOn (fn dim (iterate (step fn dim) lights !! n)) (indices dim)

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let (lights, dim) = parseLights contents
    putStrLn $ "Part 1: " ++ show (run 100 idFn dim lights)
    putStrLn $ "Part 2: " ++ show (run 100 fixCorners dim lights)
