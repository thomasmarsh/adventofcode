import Data.List (nub, transpose, maximumBy, minimumBy)
import System.Environment (getArgs)

hist :: String -> [(Char, Int)]
hist s = zip unique counts
    where unique = nub s
          counts = [length $ filter (/= c) s | c <- unique]

type Entry = (Char, Int)

find :: ((Entry -> Entry -> Ordering) -> [Entry] -> Entry)
     -> [Entry]
     -> Char
find f m = fst $ f (\(_, a) (_, b) -> compare a b) m

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let ms = map hist (transpose $ lines contents)
    putStrLn $ "Part 1: " ++ map (find maximumBy) ms
    putStrLn $ "Part 2: " ++ map (find minimumBy) ms
