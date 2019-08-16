{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Y2018.D15 where

import           Data.Bifunctor (second)
import           Data.List (intersect, sort)
import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace (trace)

type    Coord     = (Int, Int)
data    Tile      = Wall   | Open deriving (Eq, Show)
type    Cave      = M.Map Coord Tile
data    UnitType  = Goblin | Elf  deriving (Eq, Ord, Show)
newtype HP        = HP Int        deriving (Eq, Ord, Show)
data    Unit      = U Coord UnitType HP deriving (Eq, Show)
type    State     = (Cave, [Unit])

instance Ord Unit where
  (U p1 _ _) `compare` (U p2 _ _) = p1 `compare` p2

adjacent :: Coord -> [Coord]
adjacent (y,x) = [(y-1,x), (y,x-1), (y,x+1), (y+1,x)]

occupied :: [Unit] -> [Coord]
occupied = map (\(U p _ _ ) -> p) . liveUnits

occupiedBy :: [Unit] -> UnitType -> [Coord]
occupiedBy xs t = map (\(U p _ _ ) -> p) . ofType t . liveUnits $ xs

isValid :: Cave -> Coord -> Bool
isValid cave p = M.lookup p cave == Just Open

adjacentEmpty :: State -> Coord -> UnitType -> [Coord]
adjacentEmpty (cave, units) p t
    = filter (\x -> not (x `elem` withUnit)) candidates
    where
        candidates = filter (isValid cave) (adjacent p)
        withUnit   = occupiedBy units t

getPos :: Unit -> Coord
getPos (U c _ _) = c

isAlive :: Unit -> Bool
isAlive (U _ _ (HP hp)) = hp > 0

liveUnits :: [Unit] -> [Unit]
liveUnits = filter isAlive

isOfType :: UnitType -> Unit -> Bool
isOfType t1 (U _ t2 _) = t1 == t2

ofType :: UnitType -> [Unit] -> [Unit]
ofType t = filter (isOfType t)

bfs :: State -> Coord -> UnitType -> [Coord] -> [Coord]
bfs state start t targets = go [[start]] (S.singleton start)
    where
        go :: [[Coord]] -> S.Set Coord -> [Coord]
        go [] _ = []
        go (path:xs) visited
{-
            | trace
                ("--"
                 ++ "\n     path: " ++ show path
                 ++ "\n       xs: " ++ show xs
                 ++ "\n  visited: " ++ show visited
                 ++ "\n        p: " ++ show p
                 ++ "\n consider: " ++ show consider
                 ++ "\n visited': " ++ show visited'
                 ++ "\nunvisited: " ++ show unvisited
                 ++ "\nis target: " ++ show (p `elem` targets)
                ) False = undefined
-}
            | p `elem` targets = path
            | otherwise        = go (xs ++ unvisited) visited'
            where
                p         = last path
                consider  = adjacentEmpty state p t
                visited'  = S.union visited (S.fromList consider)
                unvisited = map (\x -> path++[x])
                          $ filter (\x -> not (S.member x visited)) consider

parse :: String -> State
parse s = (cave, units)
    where
        xs = [ ((y,x), col)
             | (y, row) <- zip [0..] (lines s)
             , (x, col) <- zip [0..] row ]

        toTile c     = if c == '#' then Wall else Open
        isUnit c     = c == 'E' || c == 'G'
        toUnitType c = if c == 'G' then Goblin else Elf
        toUnit (p,c) = U p (toUnitType c) (HP 200)

        cave         = M.fromList $ map (second toTile) xs
        units        = map toUnit . filter (isUnit . snd) $ xs

other :: UnitType -> UnitType
other Goblin = Elf
other Elf = Goblin

run :: String -> IO ()
run path = do
    state <- parse <$> readFile path
    print state
    print (occupied $ snd state)
    print (liveUnits $ snd state)
    let (U p t _) = last . liveUnits . snd $ state
    let targets = (map getPos . ofType (other t) . liveUnits . snd) $ state
    print (p,t)
    putStrLn $ "Targets: " ++ (show targets)
    -- mapM_ print (filter (isValid (fst state)) [(y,x) | y <- [0..31], x <- [0..31]])
    mapM_ (print . (\x -> bfs state x t targets)) $ (map getPos . ofType t . liveUnits . snd $ state)
