{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as M
import           Data.Maybe
import qualified Data.Scientific as S
import qualified Data.Vector as V
import           System.Environment

data Mode = Normal | IgnoreRed deriving (Eq)

jsum :: Mode -> String -> Int
jsum mode jsonString = jsum' $ fromJust ((decode . B.pack) jsonString :: Maybe Value)
    where jsum' (Number y) = fromIntegral $ S.coefficient y
          jsum' (Array y)  = sum (map jsum' $ V.toList y)
          jsum' (Object y) | mode == IgnoreRed && String "red" `elem` M.elems y = 0
          jsum' (Object y) = sum (map jsum' $ M.elems y)
          jsum' _ = 0

main :: IO ()
main = do
    [path] <- getArgs
    jsonString <- readFile path
    putStrLn $ "Part 1: " ++ show (jsum Normal jsonString)
    putStrLn $ "Part 2: " ++ show (jsum IgnoreRed jsonString)
