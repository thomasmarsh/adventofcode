module Y2015.D14 where

import System.Environment

runLen :: Int
runLen = 2503

type Racer = (Int, Int, Int)

data State = State { moving   :: Bool,
                     distance :: Int,
                     restTime :: Int,
                     flyTime  :: Int,
                     score    :: Int
                   } deriving (Show)

initial :: State
initial = State { moving = True,
                  distance = 0,
                  restTime = 0,
                  flyTime = 0,
                  score = 0
                }


parse :: [String] -> Racer
parse [_, "can", "fly", dist, "km/s", "for", t1, "seconds,",
       "but", "then", "must", "rest", "for", t2, "seconds."] =
            (read dist, read t1, read t2)
parse _ = error "Parse error on input."


tryStop :: Racer -> State -> State
tryStop (_, flyDur, _) state
    | flyTime state >= flyDur
        = state { moving = False, restTime = 0 }
    | otherwise = state


tryStart :: Racer -> State -> State
tryStart (_, _, restDur) state
    | restTime state >= restDur
        = state { moving = True, flyTime = 0 }
    | otherwise = state


stepRacer :: Racer -> State -> State
stepRacer racer@(speed,_,_) state
    | moving state
        = tryStop racer (state { flyTime = flyTime state + 1,
                                 distance = distance state + speed })
    | otherwise = tryStart racer (state { restTime = restTime state + 1 })


updateScores :: Int -> [State] -> [State]
updateScores high = map update'
    where update' s
            | distance s == high = s { score = score s + 1 }
            | otherwise = s


step :: [Racer] -> [State] -> [State]
step racers states = updateScores best stepped
    where stepped = zipWith stepRacer racers states
          best = maximum [distance s | s <- stepped]


findSolution :: String -> (Int, Int)
findSolution str = (bestDist, bestScore)
    where racers = map (parse . words) (lines str)
          states = replicate (length racers) initial
          final = iterate (step racers) states !! runLen
          bestDist = maximum [distance s | s <- final]
          bestScore = maximum [score s | s <- final]

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let (part1, part2) = findSolution contents
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
