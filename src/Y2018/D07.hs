module Y2018.D07 where

{-
import qualified Data.Graph as G

  {-
     5 --> 7
     |     |
     v     V
     1 --> 4 --> 8
  -}

  (myGraph,vertexToNode,keyToVertex) = G.graphFromEdges [
      ("node4",4,[8]),     -- the first component can be of any type
      ("node8",8,[]),
      ("node7",7,[4]),
      ("node5",5,[1,7]),
      ("node1",1,[4])
   ]

  sorted = map vertexToNode $ G.topSort myGraph
-}

import qualified Data.Set as S
import Data.Set (Set(..))
import qualified Data.Map as M
import Data.Map (Map(..))
import Data.Char (ord)
import Data.List (sort)
import Control.Monad (ap)

parse :: String -> Map Char (Set Char)
parse input = M.fromListWith S.union $ concat
    [[(line !! 5, S.empty), (line !! 36, S.singleton $ line !! 5)] | line <- lines input]

day7a :: String -> String
day7a = loop . parse
  where loop deps = case M.lookupMin $ M.filter S.null deps of
            Just (k, _) -> k : loop (M.map (S.delete k) $ M.delete k deps)
            _ | M.null deps -> []

day7b :: Int -> Int -> String -> Int
day7b cost workers = ap loop (map (start 0) . take workers . M.keys . M.filter S.null) . parse
  where start t c = (t + cost + ord c - ord 'A' + 1, c)
        loop deps ((t, k):ready)
          | M.null deps' && null ready' = t
          | otherwise = loop deps' ready'
          where deps' = M.map (S.delete k) $ M.delete k deps
                ok k a = k `notElem` map snd ready && S.null a
                pending = map (start t) $ M.keys $ M.filterWithKey ok deps'
                ready' = sort $ ready ++ take (workers - length ready) pending

run :: String -> IO ()
run path = do
    s <- readFile path
    putStrLn $ "Part 1: " ++ day7a s
    putStrLn $ "Part 2: " ++ (show $ day7b 60 5 s)
