module Y2017.D03 where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Direction = South | East | North | West deriving (Enum, Show, Eq)

next :: Direction -> Direction
next West = South
next x = succ x

data Bounds
    = Bounds
    { south :: Int
    , east  :: Int
    , north :: Int
    , west  :: Int
    } deriving (Show)

initBounds :: Bounds
initBounds
    = Bounds
    { south = 0 
    , east  = 1
    , north = 0
    , west  = 0 }

expandBounds :: Direction -> Bounds -> Bounds
expandBounds South bnd = bnd { south = south bnd + 1 }
expandBounds East  bnd = bnd { east  = east  bnd + 1 }
expandBounds North bnd = bnd { north = north bnd - 1 }
expandBounds West  bnd = bnd { west  = west  bnd - 1 }

data State
    = State 
    { bounds    :: Bounds
    , direction :: Direction
    , position  :: (Int, Int)
    } deriving (Show)

initState :: State
initState
    = State
    { bounds    = initBounds
    , direction = East
    , position  = (0,0)
    }

relevantBound :: State -> Int
relevantBound state
    = case direction state of
        South -> south $ bounds state
        East  -> east  $ bounds state
        North -> north $ bounds state
        West  -> west  $ bounds state

relevantComponent :: State -> Int
relevantComponent state
    = case direction state of
        South -> snd $ position state
        East  -> fst $ position state
        North -> snd $ position state
        West  -> fst $ position state

walk :: Direction -> (Int, Int) -> (Int, Int)
walk South (x, y) = (x, y+1)
walk East  (x, y) = (x+1, y)
walk North (x, y) = (x, y-1)
walk West  (x, y) = (x-1, y)

step :: State -> State
step state
    | needsTurn = turned
    | otherwise = moved
    where moved = state { position = walk (direction state) (position state) }
          needsTurn = relevantComponent moved == relevantBound moved
          nextDir = next (direction moved)
          turned = moved { direction = nextDir
                         , bounds = expandBounds nextDir (bounds moved) }

coords :: [(Int, (Int, Int))]
coords = zipWith (\i s -> (i, position s)) [1..] states
    where states = iterate step initState

distance :: Int -> Int
distance n = abs x + abs y
    where (_, (x, y)) = last $ take n coords

type SMap = M.Map (Int, Int) Int

initialMap :: SMap
initialMap = M.insert (0, 0) 1 M.empty

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) =
    [ (x-1, y-1), (x,   y-1), (x+1, y-1)
    , (x-1, y),   (x+1, y)
    , (x-1, y+1), (x,   y+1), (x+1, y+1) ]

valueOf :: SMap -> (Int, Int) -> Int
valueOf m k = fromMaybe 0 (M.lookup k m)

summedSquares :: [Int]
summedSquares = summedSquares' initialMap coords
    where summedSquares' :: SMap -> [(Int, (Int, Int))] -> [Int]
          summedSquares' _ [] = []
          summedSquares' m ((1, (0,0)):xs) = 1 : summedSquares' m xs
          summedSquares' m ((_, (x,y)):xs) = summed : summedSquares' inserted xs
            where inserted = M.insert (x, y) summed m
                  summed = sum $ map (valueOf m) (adjacent (x, y))

findFirstLargerThan :: Int -> Int
findFirstLargerThan n = head $ filter (> n) summedSquares

main :: IO ()
main = do
    let input = 289326
    print $ distance input
    print $ findFirstLargerThan input
