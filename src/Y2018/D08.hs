module Y2018.D08 where

import Debug.Trace (trace)

data License = Node [License] [Int] deriving (Show, Eq)

parseNode :: [Int] -> (License, [Int])
parseNode (n:m:rest) = (Node children meta, rest'')
    where
        (children, rest') = parseChildren n ([], rest)
        meta              = take m rest'
        rest''            = drop m rest'
        parseChildren n (cs, ns)
            | n == length cs = (cs, ns)
            | otherwise      = parseChildren n (c:cs, ns')
            where (c, ns') = parseNode ns

metadataSum :: License -> Int
metadataSum (Node children meta) = sum meta + sum (map metadataSum children)

rootValue :: License -> [Int]
rootValue x@(Node children meta)
    | trace (
        (if null children
            then ("sm=" ++ show sm)
            else ("si=" ++ show si)) ++ "; " ++ show (length children) ++ ":" ++ show indices)
        False = undefined
    | null children = sm
    | otherwise     = si
    where
        indices = filter (\x -> x > 0 && x <= length children) meta
        sm = meta
        si = concatMap (\i -> rootValue (children !! (i-1))) indices

run :: String -> IO ()
run path = do
    -- license <- fst . parseNode . map read . words <$> readFile path
    let license = fst . parseNode . map read . words $ "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
    print license
    putStrLn $ "Part 1: " ++ (show $ metadataSum license)
    putStrLn $ "Part 2: " ++ (show $ rootValue license)
