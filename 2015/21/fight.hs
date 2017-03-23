import Control.Arrow ((&&&))
import Data.List (subsequences)

data Item = Item {
    cost :: Int,
    damage :: Int,
    protection :: Int
} deriving (Show)

data Fighter = Fighter {
    hp :: Int,
    attack :: Int,
    defence :: Int
} deriving (Show)

genItems :: [(Int, Int, Int)] -> [Item]
genItems = map (\(c, d, a) -> Item { cost=c, damage=d, protection=a })

bossInitial :: Fighter
bossInitial = Fighter { hp = 104, attack = 8, defence = 1 }

player :: [Item] -> Fighter
player = foldl (\p x -> p {attack = attack p + damage x,
                           defence = defence p + protection x}) playerInitial
    where playerInitial = Fighter { hp = 100, attack = 0, defence = 0 }

choices :: [[Item]]
choices = base ++ [b ++ r | b <- base, r <- rfilt 1]
               ++ [b ++ r | b <- base, r <- rfilt 2]
    where rfilt n = filter ((== n) . length) (subsequences rings)
          base = [[w] | w <- weapons] ++ [[w,a] | w <- weapons, a <- armor]
          weapons = genItems [(8, 4, 0), (10, 5, 0), (25, 6, 0), (40, 7, 0), (74, 8, 0)]
          armor   = genItems [(13, 0, 1), (31, 0, 2), (53, 0, 3), (75, 0, 4), (102, 0, 5)]
          rings   = genItems [(25, 1, 0), (50, 2, 0), (100, 3, 0),
                               (20, 0, 1), (40, 0, 2), (80, 0, 3)]

fight :: [Item] -> Bool
fight xs = hp (fight' (player xs) bossInitial False) > 0
    where fight' a b flipped
            | hp a <= 0 || hp b <= 0 = if flipped then b else a
            | otherwise = fight' (strike a b) a (not flipped)
          strike a b = b { hp = hp b - max 1 (attack a - defence b) }

search :: (Int, Int)
search = (part1, part2)
    where results = map (fight &&& totalCost) choices
          part1 = minimum [c | (won, c) <- results, won]
          part2 = maximum [c | (won, c) <- results, not won]
          totalCost xs = sum [cost x | x <- xs]

main :: IO ()
main = do
    let (part1, part2) = search
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
