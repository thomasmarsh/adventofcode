module Y2017.D09 where

import System.Environment (getArgs)

stripCanceled :: String -> String
stripCanceled [] = []
stripCanceled [x] = [x]
stripCanceled ('!':_:xs) = stripCanceled xs
stripCanceled (x:xs) = x:stripCanceled xs

data State
    = State
    { inGarbage :: Bool
    , garbage :: Int
    , depth :: Int
    , score :: Int
    } deriving (Show)

initState :: State
initState
    = State
    { inGarbage = False
    , garbage = 0
    , score = 0
    , depth = 0 }

go :: String -> State -> State
go [] st = st
go (c:xs) st
    | inGarbage st && c /= '>'
        = go xs st { garbage = garbage st + 1 }
    | otherwise
        = case c of
            '{' -> go xs st { depth = depth st + 1 }
            '}' -> go xs st { score = score st + depth st
                            , depth = depth st - 1 }
            '>' -> go xs st { inGarbage = False }
            '<' -> go xs st { inGarbage = True }
            _   -> go xs st

run :: String -> State
run s = go (stripCanceled s) initState

main :: IO ()
main = do
    [path] <- getArgs
    s <- readFile path
    let st = run s
    print $ score st
    print $ garbage st
