module Y2018.D03 where

import qualified Data.Map as M
import qualified Data.IntSet as S
import Data.List.Split (splitOn)

type Coverage = M.Map (Int, Int) (Int, S.IntSet)

parseLine :: String -> (Int, Int, Int, Int, Int)
parseLine s = (read $ tail n, read x, read y, read w, read h)
    where
        [n, rest] = splitOn "@" s
        [a, b]    = splitOn ":" rest
        [x, y]    = splitOn "," a
        [w, h]    = splitOn "x" b

search :: Coverage
       -> S.IntSet
       -> [(Int, (Int, Int))]
       -> Int
       -> (Int, Int)
search m h [] n          = (n, head . S.toList $ h)
search m h ((i, k):ks) n = search m' h' ks n'
    where
        (v, s)  = M.findWithDefault (0, S.empty) k m
        s' = S.insert i s
        m' = M.insert k ((v+1), s') m
        n' = n + (if v == 1 then 1 else 0)
        h' | S.size s' > 1 = S.difference h s'
           | otherwise = h

parseMap :: [String] -> (Int, Int)
parseMap xs = search M.empty initialIds ps 0
    where
        ps = [ (i, (x+x', y+y'))
             | (i, x, y, w, h) <- map parseLine xs
             , x' <- [0..w-1]
             , y' <- [0..h-1]]
        initialIds = S.fromList . map fst $ ps

run :: String -> IO ()
run path = do
    (count, i) <- parseMap . lines <$> readFile path
    putStrLn $ "Part 1: " ++ (show count)
    putStrLn $ "Part 2: " ++ (show i)
