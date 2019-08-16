module Y2018.D14 where

import Data.Digits (digits, unDigits)

step :: ([Int], [Int]) -> ([Int], [Int])
step (rec, elves) = (rec', elves')
    where
        s       = sum . map (rec !!) $ elves
        rec'    = rec ++ digits 10 s
        pos e p = (e + p + 1) `mod` length rec'
        elves'  = [ (e+(rec'!!e)+1) `mod` length rec' | e <- elves]

run :: String -> IO ()
run s = do
    let inp = read s :: Int
    let xs = iterate step ([3,7], [0,1])
    let part1 = unDigits 10 . fst $ (xs !! (inp+10))
    putStrLn $ "Part 1: " ++ (show part1)
