import           Data.List (maximumBy, nub, permutations)
import           Data.List.Split (divvy)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)
import           System.Environment (getArgs)

type Weights = M.Map (String, String) Int

parseLine :: [String] -> ((String, String), Int)
parseLine [who, "would", verb, n, "happiness", "units", "by",
           "sitting", "next", "to", other] =
    ((who, init other), gainLose verb (read n))
    where
        gainLose "lose" n = -n
        gainLose "gain" n = n
        gainLose _ _ = error "bad verb"
parseLine _ = error "bad preference"

parse :: String -> Weights
parse s = M.fromList entries
    where entries = map (parseLine . words) (lines s)

guests :: Weights -> [String]
guests w = nub [a | (a, b) <- (M.keys w)]

triples :: [a] -> [[a]]
triples = (divvy 3 1) . wrap
    where wrap [] = []
          wrap [x] = [x]
          wrap [x,y] = [x,y,x]
          wrap all@(x:y:xs) = all ++ [x,y]

-- TODO: memoize to avoid seen entries
happiness :: Weights -> [String] -> ([String], Int)
happiness w g = (g, sum $ map happiness' (triples g))
    where happiness' triple
            | length triple == 3 = ((lookup l) + (lookup r))
            | otherwise = error "arity mismatch"
              where [l,p,r] = triple
                    lookup k = fromJust (M.lookup (p, k) w)

happiest :: Weights -> Int
happiest w = snd (maximumBy (comparing snd) s)
    where p = permutations (guests w)
          s = map (happiness w) p

addSelf :: Weights -> Weights
addSelf w = add' w (guests w)
    where add' y [] = y
          add' y (x:xs) = M.insert ("self", x) 0 (
                            M.insert (x, "self") 0 (
                                add' y xs))

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let w = parse contents
    putStrLn $ "Part 1: " ++ show (happiest w)
    putStrLn $ "Part 1: " ++ show (happiest (addSelf w))

