import           Data.List
import qualified Data.Map as M
import           System.Environment

type Graph = M.Map String (M.Map String Int)

-- Simplify querying the graph for an city pair
gfind ::  String -> String -> Graph -> Int
gfind a b g = maybe 0 (\x -> find' x) (M.lookup a g)
    where find' x = maybe 0 id (M.lookup b x)

-- Builds the map using an intermediate [(String,[(String,Int)])] format
buildFromList :: [(String,String,Int)] -> Graph
buildFromList xs = M.fromList [(fst y, M.fromList (snd y)) | y <- topMap]
    where cities = nub [a | (a,_,_) <- xs]
          subMap k = [(b, d) | (a, b, d) <- xs, a == k]
          topMap = [(a, subMap a) | a <- cities]

-- For a given a->b mapping, also provide the b->a entry
fillReverse :: [(String, String, Int)] -> [(String, String, Int)]
fillReverse xs = xs ++ [(b,a,d) | (a,b,d) <- xs]

-- Build the graph from the input string
parse :: String -> Graph
parse s = buildFromList $ fillReverse [parseLine ln | ln <- lines s]
    where parseLine s = (a, b, (read::String->Int) dist)
            where [a, _, b, _, dist] = words s

-- Given [1,2,3], will produce [(1,2), (2,3)]
pairs :: [a] -> [(a,a)]
pairs l = [(x,y) | (x:y:xs) <- tails l]

-- Calculates the total distance given a graph and a route
distance :: Graph -> [String] -> Int
distance g p = go $ pairs p
    where go [] = 0
          go (x:xs) = dist + (go xs)
            where dist = gfind a b g
                  (a, b) = x

-- Brute force calculation just evaluates all permutations
search :: Graph -> (Int, Int)
search g = (minimum d, maximum d)
    where paths = permutations (M.keys g)
          d = [distance g x | x <- paths]

main :: IO ()
main = do
    [path] <- getArgs
    content <- readFile path
    let g = parse content
    let (part1, part2) = search g
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
