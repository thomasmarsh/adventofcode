module Y2017.D11 where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (maximumBy)
import Data.String.Utils (rstrip)

data Pos
    = Pos
    { x :: Int
    , y :: Int
    , z :: Int
    } deriving (Eq, Show)

initPos = Pos { x = 0, y = 0, z = 0 }

delta :: String -> Pos
delta "n"  = initPos { y =  1, z = -1 }
delta "s"  = initPos { y = -1, z =  1 }
delta "ne" = initPos { x =  1, z = -1 }
delta "sw" = initPos { x = -1, z =  1 }
delta "se" = initPos { x =  1, y = -1 }
delta "nw" = initPos { x = -1, y =  1 }

move :: Pos -> Pos -> Pos
move pos d
    = pos { x = x pos + x d
          , y = y pos + y d
          , z = z pos + z d }

parse :: String -> [String]
parse = splitOn "," . rstrip

distance :: [String] -> (Int, Int)
distance steps = go initPos steps 0
    where go pos (x:xs) mp = go (move pos (delta x)) xs (mp' pos mp)
          go pos [] mp = (cubeDist pos, mp' pos mp)
          mp' pos mp = max (cubeDist pos) mp 

cubeDist :: Pos -> Int
cubeDist pos
    = maximum
    [ abs $ x pos
    , abs $ y pos
    , abs $ z pos ]

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let steps = parse contents
    print $ distance steps
    print $ fst $ distance steps
    print $ snd $ distance steps
