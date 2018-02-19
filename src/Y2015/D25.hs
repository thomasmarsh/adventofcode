module Y2015.D25 where

target :: (Int, Int)
target = (3029, 2947)

search :: Int -> Int -> Int -> Int
search x y z
    | (x, y) == target = z
    | y == 1 = search 1 (x+1) z'
    | otherwise = search (x+1) (y-1) z'
    where z' = (z * 252533) `mod` 33554393

main :: IO ()
main = print $ search 1 1 20151125
