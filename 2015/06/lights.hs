import System.Environment (getArgs)
import Data.Array ((//), (!), array, Array, elems)
import Data.List.Split (splitOn)

bound = 999

type Coord = (Int, Int)
type Lights = Array Coord Int
data Mode = OnOff | Dimmable

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

initial :: Lights
initial = array ((0, 0), (bound, bound))
                [((x, y), 0) | x <- [0..bound],
                               y <- [0..bound]]

toggle :: Mode -> Int -> Int
toggle OnOff n = 1-n
toggle Dimmable n = n+2

on :: Mode -> Int -> Int
on OnOff n = 1
on Dimmable n = n+1

off :: Mode -> Int -> Int
off OnOff n = 0 
off Dimmable n
    | n > 0 = n-1
    | otherwise = 0

update :: Lights -> (Int -> Int) -> Coord -> Coord -> Lights
update a fn (x1, y1) (x2, y2) = a // updates
    where coords = [(x,y) | x <- [x1..x2], y <- [y1..y2]]
          delta c = (c, fn (a!c))
          updates = map delta coords

count :: Lights -> Int
count a = sum $ elems a

apply :: Mode -> Lights -> [(Mode -> Int -> Int, Coord, Coord)] -> Lights
apply m a [] = a
apply m a ((fn, c1, c2):xs) = apply m (update a (fn m) c1 c2) xs

main = do
    [path] <- getArgs
    contents <- readFile path
    let commands = map parseLine (lines contents)
    let lights = initial
    putStrLn $ "Part 1: " ++ show (count (apply OnOff lights commands))
    putStrLn $ "Part 2: " ++ show (count (apply Dimmable lights commands))
