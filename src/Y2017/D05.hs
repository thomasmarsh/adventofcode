module Y2017.D05 where

import System.Environment (getArgs)

type Prog = [Int]

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs
replaceNth _ _ [] = error "empty list"

parse :: String -> Prog
parse s = map (read::String->Int) (lines s)

isDone :: Int -> Prog -> Bool
isDone pc prog = pc >= length prog || pc < 0

type StepResult = (Int, Prog, Bool)

step :: Int -> Prog -> StepResult
step pc prog = (pc', prog', done')
    where done = isDone pc prog
          jmp = prog !! pc
          pc' | done = pc
              | otherwise = pc + jmp
          prog' | done = prog
                | otherwise = replaceNth pc (jmp+1) prog
          done' = done || isDone pc' prog'

step2 :: Int -> Prog -> StepResult
step2 pc prog = (pc', prog', done')
    where done = isDone pc prog
          jmp = prog !! pc
          jmp' | jmp > 2   = jmp - 1
               | otherwise = jmp + 1
          pc' | done = pc
              | otherwise = pc + jmp
          prog' | done = prog
                | otherwise = replaceNth pc jmp' prog
          done' = done || isDone pc' prog'

run :: Prog -> [StepResult]
run = go 0
    where go pc prog = (pc', prog', done) : go pc' prog'
            where (pc', prog', done) = step pc prog

run2 :: Prog -> [StepResult]
run2 = go 0
    where go pc prog = (pc', prog', done) : go pc' prog'
            where (pc', prog', done) = step2 pc prog

runLen :: Prog -> Int
runLen prog = 1 + length (takeWhile (\ (_, _, done) -> not done) (run prog))

runLen2 :: Prog -> Int
runLen2 prog = 1 + length (takeWhile (\ (_, _, done) -> not done) (run2 prog))

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let prog = parse contents
    print $ runLen prog 
    --print $ runLen2 [0, 3, 0, 1, -3]
    print $ runLen2 prog 
