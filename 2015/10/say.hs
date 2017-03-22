import Data.List

say :: String -> String
say s = concatMap say' (group s)
    where say' t = (show . length) t ++ [head t]

rsay :: String -> Int -> Int
rsay s n = length $ foldl (\x _ -> say x) s [1..n]

main :: IO ()
main = do
    let input = "1321131112"
    putStrLn $ "Part 1: " ++ show (rsay input 40)
    putStrLn $ "Part 2: " ++ show (rsay input 50)
