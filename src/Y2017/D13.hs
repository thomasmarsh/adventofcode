module Y2017.D13 (run) where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

data Direction = Up | Down deriving (Eq, Show)

type Range    = Int
type Pos      = Int
type Severity = Int
type Time     = Int

posAtTime :: Time -> Range -> Pos
posAtTime t r = t `mod` (2 * r - 2)

severity :: [(Range, Time)] -> Severity
severity xs = sum $ fmap stepCost xs
    where
        stepCost :: (Range, Time) -> Severity
        stepCost (0, _) = 0
        stepCost (r, n)
            | posAtTime n r == 0 = n * r
            | otherwise = 0

captures :: [(Range, Time)] -> Time -> Bool
captures xs t = or $ fmap isCapture xs
    where
        isCapture :: (Range, Time) -> Bool
        isCapture (0, _) = False
        isCapture (r, n) = posAtTime (t+n) r == 0

minimumDelay :: [(Range, Time)] -> Time
minimumDelay xs = head [ t | t <- [0..], not $ captures xs t]

-- 
-- Parsing
-- 

sparsify :: Int -> [(Pos, Range)] -> [(Range, Time)]
sparsify _ [] = []
sparsify n a@((i,r):xs)
    | n == i    = (r,n):sparsify (n+1) xs
    | otherwise = (0,n):sparsify (n+1) a

parser :: Parser [(Pos, Range)]
parser = many1 (layer <* newline)
  where
      layer = (,) <$> number <*> (string ": " *> number)
      number = read <$> many1 digit

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
    result <- parseFromFile (p <* optional newline <* eof) path
    either (error . show) return result

run :: String -> IO ()
run path =
    withInput path parser >>= \parsed -> do
        let xs = sparsify 0 parsed
        putStrLn $ "Part 1: " ++ show (severity xs)
        putStrLn $ "Part 2: " ++ show (minimumDelay xs)
