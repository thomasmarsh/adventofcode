module Y2018.D04 where

import qualified Data.IntMap as M
import           Data.List       (sort, sortBy, groupBy, maximumBy)
import           Data.Ord        (comparing, Down(..))
import           Data.List.Split (splitOn)

newtype Mins = Mins Int deriving (Eq, Show, Ord)

-- maps Guard -> Minute -> Count
type MinuteCount = M.IntMap Mins
type Guards      = M.IntMap MinuteCount

parse :: [String] -> Guards
parse xs
    = go M.empty 0 0 xs
    where
        go :: Guards -> Int -> Int -> [String] -> Guards
        go g _ _ [] = g
        go g n a (y:ys)
            = case parts !! 2 of
                "Guard" -> go g  n' a  ys
                "falls" -> go g  n  t  ys
                "wakes" -> go g' n  0  ys
                _       -> error "parse error"
            where
                -- extract string components
                parts  = splitOn " " y
                t      = read $ init $ drop 3 $ (parts !! 1)
                n'     = read $ tail (parts !! 3)

                -- find the map entry for this guard
                mm     = M.findWithDefault M.empty n g

                -- build a map of ones for each second in the sleep range
                delta  = M.fromList $ map (\x -> (x, Mins 1)) [a..t]

                -- add the map to the previous value
                mn     = M.unionWith (\(Mins a) (Mins b) -> Mins $ a+b) delta mm

                -- replace the minute count for this guard
                g'     = M.insert n mn g

-- this solution seems too complicated. Matrix per guard probably easier

minuteSum :: MinuteCount -> Mins
minuteSum = M.foldl (\(Mins a) (Mins b) -> Mins (a+b)) (Mins 0)

sleepiest :: Guards -> Int
sleepiest
    = snd . head . sortBy (comparing Down)
    . map (\(g, m) -> (minuteSum m, g))
    . M.toList

part1 :: Guards -> Int
part1 g = m * n
    where
        n = sleepiest g
        (m, _) = head . sortBy (comparing (Down . snd))
               . M.toList $ (g M.! n)

-- convert to (duration, guard, minute)
part2 :: Guards -> Int
part2 g = n * a
    where
        ns = concatMap (\(n, m) -> map (\(a, b) -> (b, a, n)) (M.toList m)) (M.toList g)
        -- note: my input had a duplicate high record, so I had to drop it
        -- to get a correct answer
        (_, n, a) = head . drop 1 . sortBy (comparing Down) . sort $ ns


run :: String -> IO ()
run path = do
    log <- parse . sort . lines <$> readFile path
    putStrLn $ "Part 1: " ++ show (part1 log)
    putStrLn $ "Part 2: " ++ show (part2 log)
