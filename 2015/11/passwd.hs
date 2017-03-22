import           Data.Char
import           Data.List
import qualified Data.Set as S

len :: Int
len = 8

p2n :: String -> Int
p2n p = sum [n c i | (c, i) <- zip (reverse p) [0..((length p)-1)]]
    where n c i = ((ord c) - (ord 'a')) * (26 ^ i)

n2p :: Int -> String
n2p n = prefix ++ p
    where p = reverse $ go n
          prefix = (take (len-(length p)) . repeat) 'a'
          c m = chr ((m `mod` 26) + (ord 'a'))
          go 0 = ""
          go m = [c m] ++ go (m `div` 26)

window :: Int -> [a] -> [[a]]
window n = foldr (zipWith (:)) (repeat []) . take n . tails

isStraight :: String -> Bool
isStraight p = or [and [b == a+1,
                        c == b+1] | [a,b,c] <- window 3 ns]
    where ns = map p2n [[c] | c <- p]

validChars :: String -> Bool
validChars p = not $ or [elem c p | c <- "iol"]

pairs :: Eq a => [a] -> [a]
pairs [] = []
pairs [_] = []
pairs (x:y:xs) = if x == y
                 then [x] ++ pairs xs
                 else pairs (y:xs)

hasPairs :: String -> Bool
hasPairs p = (S.size s) > 1
    where s = (S.fromAscList . sort . pairs) p

isValid :: String -> Bool
isValid p = and [length p == len,
                 validChars p,
                 isStraight p,
                 hasPairs p]

next :: String -> String
next p = n2p ((p2n p)+1)

replaceInvalid :: String -> String
replaceInvalid p = map repl p
    where repl 'i' = 'j'
          repl 'o' = 'p'
          repl 'l' = 'm'
          repl c = c

nextValid :: String -> String
nextValid p = go $ next p
    where go q
            | isValid q = q
            | otherwise = go $ replaceInvalid (next q)

main :: IO ()
main = do
    let p = nextValid "cqjxjnds"
    putStrLn $ "Part 1: " ++ p
    putStrLn $ "Part 2: " ++ (nextValid p)
