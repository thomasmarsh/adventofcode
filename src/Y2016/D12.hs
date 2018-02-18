module Y2016.D12 where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- Types

data State = State
    { rA :: Int
    , rB :: Int
    , rC :: Int
    , rD :: Int
    , pc :: Int
    } deriving (Show)

data Register
    = A | B | C | D
    deriving (Eq, Show)

data Descriptor
    = Reg Register
    | Val Int
    deriving (Eq, Show)

data Instruction
    = Cpy Descriptor Register
    | Inc Register
    | Dec Register
    | Jnz Descriptor Int
    deriving (Eq, Show)

type Program = [Instruction]

-- Parsing

readInt :: String -> Int
readInt s = fromMaybe 0 (readMaybe s :: Maybe Int)

parseReg :: String -> Register
parseReg s =
    case s of
        "a" -> A
        "b" -> B
        "c" -> C
        "d" -> D
        _ -> error "bad register"

isRegister :: String -> Bool
isRegister s = s `isInfixOf` "abcd"

parseLine :: [String] -> Instruction
parseLine ["cpy", x, y]
    | isRegister x = Cpy (Reg (parseReg x)) (parseReg y)
    | otherwise = Cpy (Val (readInt x)) (parseReg y)
parseLine ["inc", x] = Inc (parseReg x)
parseLine ["dec", x] = Dec (parseReg x)
parseLine ["jnz", x, y]
    | isRegister x = Jnz (Reg (parseReg x)) (readInt y)
    | otherwise = Jnz (Val (readInt x)) (readInt y)
parseLine _ = error "parse error"

parse :: String -> Program
parse s = map (parseLine . splitOn " ") (lines s)

-- Execution

setReg :: State -> Register -> Int -> State
setReg st A n = st { rA = n }
setReg st B n = st { rB = n }
setReg st C n = st { rC = n }
setReg st D n = st { rD = n }

getReg :: State -> Register -> Int
getReg st A = rA st
getReg st B = rB st
getReg st C = rC st
getReg st D = rD st

incPc :: State -> State
incPc st = st { pc = pc st + 1 }

doCpy :: State -> Register -> Int -> State
doCpy st r n = incPc $ setReg st r n

doJnz :: State -> Int -> Int -> State
doJnz st q y
    | q /= 0 = st { pc = pc st + y }
    | otherwise = incPc st

exec :: State -> Instruction -> State
exec st (Cpy (Reg x) y) = doCpy st y (getReg st x)
exec st (Cpy (Val x) y) = doCpy st y x
exec st (Inc r)         = incPc $ setReg st r (getReg st r + 1)
exec st (Dec r)         = incPc $ setReg st r (getReg st r - 1)
exec st (Jnz (Reg x) n) = doJnz st (getReg st x) n
exec st (Jnz (Val x) n) = doJnz st x n

initial :: State
initial = State { rA = 0, rB = 0, rC = 0, rD = 0, pc = 0 }

run :: Program -> State -> Int
run prog start = rA $ go start
    where go st
            | pc st < 0 || pc st > (length prog -1) = st
            | otherwise = go $ exec st (prog !! pc st)

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let prog = parse contents
    putStrLn $ "Part 1: " ++ show (run prog initial)
    putStrLn $ "Part 2: " ++ show (run prog (initial { rC = 1 }))
