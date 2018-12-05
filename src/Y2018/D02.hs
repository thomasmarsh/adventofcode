module Y2018.D02 where

import Data.List (sort, group, tails)
import Data.Maybe (catMaybes)

checksum :: [String] -> Int
checksum xs = count 2 * count 3
    where
        lengths = map length . group . sort
        ys = map lengths xs
        count n = length $ filter (elem n) ys

matchPair :: (String, String) -> Maybe String
matchPair (a, b)
    | length a /= length b     = error "length mismatch"
    | length s == length a - 1 = Just s
    | otherwise                = Nothing
    where
        s = map fst $ filter (\(x, y) -> x == y) (zip a b)

findClosest :: [String] -> String
findClosest xs
    -- n.b.: Safe.headMay
    = head $ catMaybes $ map matchPair pairs
    where
        pairs = [(x,y) | (x:ys) <- tails xs, y <- ys]

run :: String -> IO ()
run path = do
    xs <- lines <$> readFile path
    putStrLn $ "Part 1: " ++ (show $ checksum xs)
    putStrLn $ "Part 2: " ++ findClosest xs
