module Y2018.D13 where

import           Control.Arrow (second)
import           Data.List     (sort)
import qualified Data.Map   as M
import           Data.Maybe    (catMaybes, isNothing, fromJust)
import qualified Data.Set   as S
import           Data.Tuple    (swap)

type Coord     = (Int, Int)
data Turn      = L | A | R              deriving (Enum, Eq, Ord, Show)
data Direction = N | E | S | W          deriving (Eq, Ord, Show)
data Rail      = V | H | F | B | X      deriving (Eq, Ord, Show)
data Cart      = C Coord Turn Direction deriving (Eq, Ord, Show)
type Rails     = M.Map Coord Rail

nextTurn :: Turn -> Turn
nextTurn x = if x == R then L else succ x

turn :: Turn -> Direction -> Direction
turn L N = W;  turn L E = N;  turn L S = E;  turn L W = S
turn R N = E;  turn R E = S;  turn R S = W;  turn R W = N
turn A x = x

stepPos :: Direction -> Coord -> Coord
stepPos d (y,x) = case d of
    N -> (y-1, x);  E -> (y, x+1)
    S -> (y+1, x);  W -> (y, x-1)

stepDir :: (Direction, Rail) -> Direction
stepDir x = case x of
    (N, V) -> N;  (S, V) -> S;  (E, H) -> E;  (W, H) -> W
    (N, F) -> E;  (S, F) -> W;  (E, F) -> N;  (W, F) -> S
    (N, B) -> W;  (S, B) -> E;  (E, B) -> S;  (W, B) -> N

stepCartDir :: Cart -> Rail -> Cart
stepCartDir (C p t d) X = C p (nextTurn t) (turn t d)
stepCartDir (C p t d) r = C p t            (stepDir (d, r))

stepCart :: Rails -> Cart -> Cart
stepCart m (C p t d) = let pos' = stepPos d p
                       in stepCartDir (C pos' t d) (fromJust $ M.lookup pos' m)

moveCarts :: Rails -> [Cart] -> [Cart] -> [Coord] -> ([Cart], [Coord])
moveCarts _ done []       c = (sort done, c)
moveCarts m done (x:rest) c
    | collided  = moveCarts m (filtP done) (filtP rest) (p:c)
    | otherwise = moveCarts m (x':done)    rest         c
    where x'@(C p _ _) = stepCart m x
          filtP        = filter (\(C pos _ _) -> pos /= p)
          collided     = p `elem` (map (\(C pos _ _) -> pos) (done ++ rest))

step :: ([Cart], Rails, [Coord]) -> ([Cart], Rails, [Coord])
step (cs, m, c) = (cs', m, c')
    where
        (cs', c') = moveCarts m [] cs c

parse :: String -> ([Cart], Rails)
parse s = ( sort . catMaybes . map parseCart $ xs
          , M.fromList $ map (second parseRail) xs)
    where xs = [ ((y, x), c)
               | (row, y) <- zip (lines s) [0..]
               , (c,   x) <- zip row       [0..]
               , c /= ' ' ]
          parseCart (p, c) = case c of
              '^' -> Just $ C p L N; '>' -> Just $ C p L E
              'v' -> Just $ C p L S; '<' -> Just $ C p L W
              _   -> Nothing
          parseRail c = case c of
              '|' -> V; '^' -> V; 'v' -> V
              '>' -> H; '<' -> H; '-' -> H
              '+' -> X; '/' -> F; '\\' -> B

part1, part2 :: [([Cart], Rails, [Coord])] -> Coord

part1 = head . (\(_,_,x) -> x)
      . head . filter (\(_,_,x) -> length x == 1)

part2 = (\(C p _ _) -> p)
      . head . (\(x,_,_) -> x)
      . head . filter (\(x,_,_) -> length x == 1)

run :: String -> IO ()
run path = do
    (cs, m) <- parse <$> readFile path
    let xs = iterate step (cs, m, [])
    putStrLn $ "Part 1: " ++ (show . swap . part1 $ xs)
    putStrLn $ "Part 2: " ++ (show . swap . part2 $ xs)
