{-# LANGUAGE ViewPatterns #-}

module Y2017.D12 where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import System.Environment (getArgs)

parse :: String -> M.Map Int [Int]
parse = M.fromList . map parseLine . lines where
    parseLine (words -> x : "<->" : xs) = (read x, read . filter isDigit <$> xs)

{-  WIP
connected :: (Ord a) => M.Map a [a] -> a -> S.Set a
connected neighbors = grow S.empty . (:[]) where
    grow s [] = s
    grow s (x:xs) = grow (S.insert x s) $ queue ++ xs where
        queue = if S.member x s then [] 
                else fromMaybe [] $ neighbors M.(!?) x

day12a :: String -> Int
day12a = S.size . flip connected 0 . parse

day12b :: String -> Int
day12b input = length . unfoldr (fmap dropConnected . S.minView) $
               M.keysSet neighbors where
    neighbors = parse input
    dropConnected (x, xs) = ((), xs S.\\ connected neighbors x)
-}

main = do
    [path] <- getArgs
    contents <- readFile path
    print $ parse contents
