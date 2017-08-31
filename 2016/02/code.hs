import System.Environment (getArgs)

type Pos = (Int, Int)
type Keypad = [String]

isValid :: Keypad -> Pos -> Bool
isValid pad (x, y) =
    not $ x < 0 || y < 0 ||
          x >= length pad ||
          y >= length (head pad) ||
          ((pad !! y) !! x) == '*'

charAt :: Keypad -> Pos -> Char
charAt pad (x, y)
    | not $ isValid pad (x, y) = error "invalid key"
    | otherwise = (pad !! y) !! x

move :: Pos -> Char -> Pos
move (x, y) d
    | d == 'U' = (x, y-1)
    | d == 'D' = (x, y+1)
    | d == 'L' = (x-1, y)
    | d == 'R' = (x+1, y)
    | otherwise = error "unexpected direction"

nextPos :: Keypad -> Pos -> Char -> Pos
nextPos pad pos d
    | isValid pad pos' = pos'
    | otherwise = pos
    where pos' = move pos d

solveKey :: Keypad -> Pos -> String -> Pos
solveKey pad pos [] = pos
solveKey pad pos (x:xs) = solveKey pad (nextPos pad pos x) xs

solve :: Keypad -> Pos -> [String] -> String
solve pad pos ss = map (charAt pad) $ go pos ss []
    where go :: Pos -> [String] -> [Pos] -> [Pos]
          go pos' [] result = reverse result
          go pos' (x:xs) result = go pos'' xs (pos'' : result)
            where pos'' = solveKey pad pos' x

keypad1 = ["123",
           "456",
           "789"]

keypad2 = ["**1**",
           "*234*",
           "56789",
           "*ABC*",
           "**D**"]

main :: IO ()
main = do
    [fname] <- getArgs
    contents <- readFile fname
    let ss = lines contents
    putStrLn $ "Part 1: " ++ solve keypad1 (1, 1) ss
    putStrLn $ "Part 2: " ++ solve keypad2 (0, 2) ss
