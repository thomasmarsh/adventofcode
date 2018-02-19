module Y2016.D19 where

-- This is the Josephus problem: https://en.wikipedia.org/wiki/Josephus_problem

import Data.Char (intToDigit, digitToInt)
import Data.Maybe (listToMaybe, fromJust)
import Numeric (showIntAtBase, readInt)

showBin :: Int -> String
showBin n = showIntAtBase 2 intToDigit n ""

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

josephus :: Int -> Int
josephus n = fromJust (readBin $ showBin n ++ "1")

josephus3 :: Int -> Int
josephus3 n = n - max (n-2*p) 0
    where p = floor $ 3 ^ q
          q :: Int
          q = floor (logBase (3::Double) (fromIntegral (n-1)))

main :: IO ()
main = do
    putStrLn $ "Part 1: " ++ show (josephus 3005290)
    putStrLn $ "Part 2: " ++ show (josephus3 3005290)
