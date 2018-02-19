module Y2017.D10 where

import System.Environment (getArgs)
import Data.List (foldl', unfoldr)
import Data.Bits (xor)
import Data.Char (ord)
import Text.Printf (printf)
import Data.Bool.Extras (bool)

-- not my solution

roll :: Int -> [a] -> (Int, Int) -> [a]
roll _ list (count, skip) = end ++ start where
    (knot, keep) = splitAt count list
    (start, end) = splitAt skip $ keep ++ reverse knot

rolls :: Int -> [Int] -> [Int]
rolls len counts = end ++ start where
    list = [0 .. len - 1]
    rotated = foldl (roll len) list . zip counts $ cycle list
    rotation = sum (zipWith (+) [0..] counts) `mod` len
    (start, end) = splitAt (len - rotation) rotated

day10a :: String -> Int
day10a input = let x:y:_ = rolls 256 . read $ '[' : input ++ "]" in x * y

day10b :: String -> String
day10b input = map (foldl' xor 0) dense >>= printf "%02x" where
    sparse = rolls 256 . concat . replicate 64 $
        map ord (concat $ lines input) ++ [17, 31, 73, 47, 23]
    dense = unfoldr (bool (Just . splitAt 16) (const Nothing) =<< null) sparse

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    print $ day10a contents
    print $ day10b contents
