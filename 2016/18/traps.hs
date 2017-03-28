import Data.List (tails)

count :: (a -> Bool) -> [a] -> Int
count p xs = length $ filter p xs

rule :: Char -> Char -> Char
rule x y
    | x == y = '.'
    | otherwise = '^'

next :: String -> String
next pat = [rule x y | x:_:y:_ <- tails ("." ++ pat ++ ".")]

search :: String -> Int -> Int
search pat n = count ('.'==) $ concat $ take n $ iterate next pat

initial :: String
initial = ".^^^^^.^^.^^^.^...^..^^.^.^..^^^^^^^^^^..^...^^.^..^^^^..^^^^...^.^.^^^^^^^^....^..^^^^^^.^^^.^^^.^^"

main :: IO ()
main = do
    putStrLn $ "Part 1: " ++ show (search initial 40)
    putStrLn $ "Part 2: " ++ show (search initial 400000)
