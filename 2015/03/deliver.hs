import System.Environment
import Data.List

move c (x,y)
    | c == '^' = (x,y+1)
    | c == 'v' = (x,y-1)
    | c == '<' = (x-1,y)
    | c == '>' = (x+1,y)
    | otherwise = (x,y)

houses route = scanl (\x y -> move y x) (0,0) route
unique xs = length . group $ sort xs

each n = map head . takeWhile (not . null) . iterate (drop n)
everyOther = each 2

deliver route = unique $ houses route

deliver2 route = unique (santa ++ robot)
    where santa = houses $ everyOther route
          robot = houses $ everyOther (tail route)

main = do
    args <- getArgs
    content <- readFile (args !! 0)
    putStrLn $ "Part 1: " ++ show (deliver content)
    putStrLn $ "Part 2: " ++ show (deliver2 content)
