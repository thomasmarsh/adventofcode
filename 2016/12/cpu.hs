import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

data State = State
    { rA :: Int
    , rB :: Int
    , rC :: Int
    , rD :: Int
    , pc :: Int
    } deriving (Show)

isRegister :: String -> Bool
isRegister s = s `isInfixOf` "abcd"

setReg :: State -> String -> Int -> State
setReg state r n = case r of
                    "a" -> state { rA = n }
                    "b" -> state { rB = n }
                    "c" -> state { rC = n }
                    "d" -> state { rD = n }

getReg :: State -> String -> Int
getReg state r = case r of
                    "a" -> rA state
                    "b" -> rB state
                    "c" -> rC state
                    "d" -> rD state

incPc :: State -> State
incPc state = state { pc = pc state + 1 }

readInt :: String -> Int
readInt s = fromMaybe 0 (readMaybe s :: Maybe Int)

cpy :: State -> [String] -> State
cpy state [x, y]
    | isRegister x = go (getReg state)
    | otherwise = go readInt
    where go f = incPc $ setReg state y (f x)

inc :: State -> [String] -> State
inc state [r] = incPc $ setReg state r (getReg state r + 1)

dec :: State -> [String] -> State
dec state [r] = incPc $ setReg state r (getReg state r - 1)

jnz :: State -> [String] -> State
jnz state [x, y]
    | q /= 0 = state { pc = pc state + readInt y }
    | otherwise = incPc state
    where q = if isRegister x
              then getReg state x
              else readInt y

exec :: State -> [String] -> State
exec state (x:xs) =
    case x of 
        "cpy" -> cpy state xs
        "inc" -> inc state xs
        "dec" -> dec state xs
        "jnz" -> jnz state xs

initial :: State
initial = State { rA = 0, rB = 0, rC = 0, rD = 0, pc = 0 }

run :: [[String]] -> State -> Int
run prog init = rA $ go init
    where go state
            | pc state < 0 || pc state > (length prog -1) = state
            | otherwise = go $ exec state (prog !! pc state)

main :: IO ()
main = do
    [path] <- getArgs
    contents <- readFile path
    let prog = map (splitOn " ") (lines contents)
    putStrLn $ "Part 1: " ++ show (run prog initial)
    putStrLn $ "Part 2: " ++ show (run prog (initial { rC = 1 }))
