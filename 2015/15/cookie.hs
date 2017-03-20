import System.Environment (getArgs)
import Data.List (transpose, maximumBy)
import Data.Ord (comparing)

-- Parsing

parseLine :: [String] -> [Int]
parseLine [_, "capacity", a,
              "durability", b,
              "flavor", c, 
              "texture", d,
              "calories", e] = [x a, x b, x c, x d, read e::Int]
    where x s = (read $ init s::Int)

parse :: String -> [[Int]]
parse s = map (parseLine . words) (lines s)

-- Scoring

distribute :: [[Int]] -> [Int] -> [[Int]]
distribute props measures = [map (* (snd x)) (fst x)
                                    | x <- zip props measures]

eval :: [[Int]] -> [Int] -> (Int, Int)
eval props measures = (score, calories)
    where d = distribute props measures
          calories = sum $ map last d
          score = foldr1 (*) $ ((map relu) . map sum . transpose) (map init d)
          relu n | n < 0 = 0 | otherwise = n 


-- Searching and Ranking

amounts :: [[Int]]
amounts = [[i,j,k,n-(i+j+k)] | i <- [0..n],
                               j <- [0..n-i],
                               k <- [0..n-i-j]]
    where n = 100

evalMany :: [[Int]] -> [[Int]] -> [(Int, Int)]
evalMany props ms = map (eval props) ms

solution :: [[Int]] -> (Int, Int)
solution props = (fst $ high rs, fst $ high nutritional)
    where rs = evalMany props amounts
          high xs = maximumBy (comparing fst) xs
          nutritional = filter ((==500) . snd) rs

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let props = parse contents
    let (part1, part2) = solution props
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
