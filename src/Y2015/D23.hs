module Y2015.D23 where

import System.Environment (getArgs)

data Reg = A | B

type RegVal = Integer

data CPU = CPU {
    rA :: RegVal,
    rB :: RegVal,
    pc :: Int
} deriving (Show)

initialState :: CPU
initialState = CPU { rA = 0, rB = 0, pc = 0 }

done ::  CPU -> [[String]] -> Bool
done cpu prog = pc cpu >= length prog || pc cpu < 0

incPc :: CPU -> CPU
incPc cpu = cpu { pc = 1 + pc cpu }

updateReg :: CPU -> (RegVal -> RegVal) -> Reg -> CPU
updateReg cpu fn A = incPc cpu { rA = fn (rA cpu) }
updateReg cpu fn B = incPc cpu { rB = fn (rB cpu) }

getReg :: CPU -> Reg -> RegVal
getReg cpu A = rA cpu
getReg cpu B = rA cpu

jmp :: CPU -> Int -> CPU
jmp cpu i = cpu { pc = pc cpu + i }

jie :: CPU -> Reg -> Int -> CPU
jie cpu r i
    | (even . getReg cpu) r = jmp cpu i
    | otherwise = incPc cpu

jio :: CPU -> Reg -> Int -> CPU
jio cpu r i
    | getReg cpu r == 1 = jmp cpu i
    | otherwise = incPc cpu

apply :: CPU -> [String] -> CPU
apply cpu = parse
    where parse :: [String] -> CPU
          parse ["hlf", r]    = updateReg cpu (`quot` 2) (parseReg r)
          parse ["tpl", r]    = updateReg cpu (*3) (parseReg r)
          parse ["inc", r]    = updateReg cpu (+1) (parseReg r)
          parse ["jmp", i]    = jmp cpu (parseInt i)
          parse ["jie", r, i] = jie cpu (parseReg r) (parseInt i)
          parse ["jio", r, i] = jio cpu (parseReg r) (parseInt i)
          parse _ = error "bad instruction"

          parseInt :: String -> Int
          parseInt (prefix:s)
            | prefix == '+' = (read :: String -> Int) s
            | prefix == '-' = negate ((read :: String -> Int) s)
            | otherwise = error "bad number prefix"
          parseInt [] = error "empty string"

          parseReg :: String -> Reg
          parseReg r
            | fixR == "a" = A
            | fixR == "b" = B
            | otherwise = error "bad register"
            where fixR = (filter . flip notElem) "," r

run :: (RegVal, RegVal) -> [[String]] -> (RegVal, RegVal)
run (a, b) prog = run' initial
    where initial = initialState { rA = a, rB = b }
          run' cpu
            | done cpu prog = (rA cpu, rB cpu)
            | otherwise = run' (apply cpu (prog !! pc cpu))

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let prog = map words (lines contents)
    let (_, part1) = run (0, 0) prog
    let (_, part2) = run (1, 0) prog
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
