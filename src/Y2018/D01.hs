module Y2018.D01 where

import           Data.IntSet (IntSet)
import qualified Data.IntSet as S

parseNum :: String -> Int
parseNum x =
    case head x of
        '+' -> read $ tail x
        _ -> read x

repeatedFreq :: [Int] -> Int
repeatedFreq xs = findDup S.empty xs'
    where
        xs' = scanl1 (+) (cycle xs)
        findDup :: IntSet -> [Int] -> Int
        findDup z (y:ys)
            | S.member y z = y
            | otherwise    = findDup (S.insert y z) ys

run :: String -> IO ()
run path = do
    ls <- map parseNum . lines <$> readFile path
    putStrLn $ "Part 1: " ++ (show $ sum ls)
    putStrLn $ "Part 2: " ++ (show $ repeatedFreq ls)
