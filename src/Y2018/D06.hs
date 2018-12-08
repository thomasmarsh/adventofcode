module Y2018.D06 where

-- spent too much time spinning on this one because there was a bug
-- in the AOC input generator (affected 20% of users)

import Data.Function   (on)
import Data.Ix         (range)
import Data.List       (groupBy, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe      (fromJust, isJust)
import Data.Ord        (comparing)

type Point   = (Int, Int)
type Rect    = (Point, Point)
type Voronoi = [(Point, [Point])]

bbox :: [Point] -> Rect
bbox zs
    = ((ax, ay), (bx, by))
    where
        (xs, ys) = (map fst zs, map snd zs)
        (ax, bx) = (minimum xs, maximum xs)
        (ay, by) = (minimum ys, maximum ys)

dist :: Point -> Point -> Int
dist (ax, ay) (bx, by) = abs (bx-ax) + abs (by-ay)

closest :: [Point] -> Point -> Maybe Point
closest cs c
    | length ds == 1 = Just (snd $ head ds)
    | otherwise      = Nothing
    where
        ds = head . groupBy ((==) `on` fst)
           . sort . zip (map (dist c) cs) $ cs

voronoi :: [Point] -> (Voronoi, Rect)
voronoi cs = (vs, b)
    where
        b@((ax, ay), (bx, by)) = bbox cs
        range     = [(x,y) | x <- [ax..bx], y <- [ay..by]]
        vs = map (\x -> (snd . head $ x, map fst x))
           . groupBy ((==) `on` snd)
           . sortBy (comparing snd)
           . map (\(a, b) -> (a, fromJust b))
           . filter (isJust . snd)
           $ (zip range $ map (closest cs) range)

largestFinite :: [Point] -> Int
largestFinite cs = maximum $ map (length . snd) finite
    where
        (vs, ((ax, ay), (bx, by))) = voronoi cs
        isEdge (x,y) = x == ax || x == bx || y == ay || y == by
        edges  = map (\(_, cs) -> or $ map isEdge cs) vs
        finite = map fst $ filter snd $ zip vs edges

findRegion :: [Point] -> Int
findRegion cs
    = length
    . filter (\x -> sum (map (dist x) cs) < n)
    . within (n `div` length cs) $ cs
    where
        n = 10000
        within w ps = range ((ax, ay), (bx, by))
            where ax = minimum (map fst ps) - w
                  ay = minimum (map snd ps) - w
                  bx = maximum (map fst ps) + w
                  by = maximum (map snd ps) + w

run :: String -> IO ()
run path = do
    cs <- map parseLine . lines <$> readFile path
    putStrLn $ "Part 1: " ++ (show $ largestFinite cs)
    putStrLn $ "Part 2: " ++ (show $ findRegion cs)
    where
        parseLine = (\[a,b] -> (a,b)) . map read . splitOn ", "

