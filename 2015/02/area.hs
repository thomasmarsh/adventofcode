import System.Environment
import Data.List
import Data.List.Split

area dim = 2*l*w + 2*w*h + 2*h*l
    where l = dim !! 0
          w = dim !! 1
          h = dim !! 2

smallest dim = take 2 $ sort dim

paper dim = (area dim) + (product $ smallest dim)
ribbon dim = perim + bow
    where perim = sum $ map (*2) (smallest dim)
          bow = product dim

parseLine s = map (read::String->Int) $ splitOn "x" s

part1 lines = sum . map paper $ map parseLine lines
part2 lines = sum . map ribbon $ map parseLine lines

main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let l = lines content
   putStrLn $ "Part 1: " ++ show (part1 l)
   putStrLn $ "Part 1: " ++ show (part2 l)
