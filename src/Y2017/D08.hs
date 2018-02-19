module Y2017.D08 where

import qualified Data.Map as M
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)

type Registers = M.Map String Int

getVal :: Registers -> String -> Int
getVal r k = fromMaybe 0 (M.lookup k r)

setVal :: Registers -> String -> (Int -> Int) -> Registers
setVal r k fn = M.insert k (fn $ getVal r k) r

data Op = Inc | Dec deriving (Show)
data Cmp = CLT | CGT | CEQ | CNEQ | CGTEQ | CLTEQ deriving (Show)
type Instruction = (String, Op, Int, String, Cmp, Int)
type Prog = [Instruction]

parseOp :: String -> Op
parseOp "inc" = Inc
parseOp "dec" = Dec
parseOp _ = error "parse instr error"

parseCmp :: String -> Cmp
parseCmp "<" = CLT
parseCmp ">" = CGT
parseCmp "==" = CEQ
parseCmp "!=" = CNEQ
parseCmp ">=" = CGTEQ
parseCmp "<=" = CLTEQ
parseCmp _ = error "parse op error"

parseWords :: [String] -> Instruction
parseWords [r1,op,delta,"if",r2,cmp,rhs]
    = (r1
    , parseOp op
    , (read::String->Int) delta
    , r2
    , parseCmp cmp
    , (read::String->Int) rhs)
parseWords _ = error "bad parse"

parse :: String -> Prog
parse s = map (parseWords . words) (lines s)

testCond :: Int -> Cmp -> Int -> Bool
testCond a CLT b = a < b
testCond a CGT b = a > b
testCond a CEQ b = a == b
testCond a CNEQ b = a /= b
testCond a CGTEQ b = a >= b
testCond a CLTEQ b = a <= b

applyOp :: Int -> Op -> Int -> Int
applyOp a Inc b = a + b
applyOp a Dec b = a - b

exec :: Registers -> Instruction -> Registers
exec r (r1, op, delta, r2, cmp, rhs)
    | fulfilled = updated
    | otherwise = r
    where fulfilled = testCond (getVal r r2) cmp rhs
          updated = M.insert r1 (applyOp (getVal r r1) op delta) r

run :: Prog -> (Registers, Int)
run prog = go M.empty prog 0
    where go r [] hi = (r, hi)
          go r (instr:xs) hi = go r' xs hi'
            where r' = exec r instr
                  hi' = max hi (highestVal r')

highestVal :: Registers -> Int
highestVal r = maximum $ M.elems r

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let prog = parse contents
    print $ (highestVal . fst . run) prog
    print $ (snd . run) prog
