module Y2018.D11 where

import qualified Data.Map as M
import Data.Ord (comparing)
import Data.List (maximumBy)

type Point = (Int, Int)
type LevelMap = M.Map Point Int

powerLevel :: Int -> Point -> Int
powerLevel serialNo (x,y)
    = b - 5
    where
        rackId = x + 10
        a = (rackId * y + serialNo) * rackId
        b = (a `mod` 1000) `div` 100

powerLevels :: Int -> LevelMap
powerLevels serialNo = M.fromList $ zip cells ps
    where
        cells = [(x,y) | x <- [1..300], y <- [1..300]]
        ps = map (powerLevel serialNo) cells

squares :: Int -> [[Point]]
squares w
    = [ [(x+dx, y+dy) | dx <- [0..w-1] , dy <- [0..w-1]]
      | x <- [1..300-(w-1)]
      , y <- [1..300-(w-1)]]

squarePower :: LevelMap -> [Point] -> Int
squarePower m ps = sum $ map (m M.!) ps

maxPower :: Int -> Int -> ((Int, Int), Int)
maxPower serialNo w
    = (head $ fst best, snd best)
    where
        m = powerLevels serialNo
        s = squares w
        z = zip s $ (map (squarePower m)) s
        best = maximumBy (comparing snd) $ z

run :: String -> IO ()
run s = do
    let serialNo = read s
    putStrLn $ "Part 1: " ++ (show $ maxPower serialNo 3)
    putStrLn "Part 2:"
    mapM_ print ([(c,level,w)
        | w <- [1..299]
        , let (c,level) = maxPower serialNo w])
