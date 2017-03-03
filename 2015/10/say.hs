import Data.List

say :: String -> String
say s = concat (map say' (group s))
    where say' t = ((show . length) t) ++ [t!!0]

rsay :: String -> Int -> Int
rsay s n = length $ foldl (\x y -> say x) s [1..n]

main = do
    --print $ say "1" -- "11"
    --print $ say "11" -- "21"
    --print $ say "21" -- "1211"
    --print $ say "1211" -- "111221"
    --print $ say "111221" -- "312211"
    let input = "1321131112"
    putStrLn $ "Part 1: " ++ (show (rsay input 40))
    putStrLn $ "Part 2: " ++ (show (rsay input 50))
