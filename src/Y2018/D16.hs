{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Y2018.D16 where

import Data.Bits ((.&.), (.|.))
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (permutations)
import Control.Monad (guard)

data Regs      = R Int Int Int Int   deriving (Eq, Show)
data RIx      = R0 | R1 | R2 | R3    deriving (Enum, Eq, Show)
type RawOp    = (Int, Int, Int, Int)
type Sample   = (Regs, RawOp, Regs)

data Op = Addr RIx RIx RIx | Addi RIx Int RIx
        | Mulr RIx RIx RIx | Muli RIx Int RIx
        | Banr RIx RIx RIx | Bani RIx Int RIx
        | Borr RIx RIx RIx | Bori RIx Int RIx
        | Setr RIx RIx     | Seti Int RIx
        | Gtir Int RIx RIx | Gtri RIx Int RIx | Gtrr RIx RIx RIx
        | Eqir Int RIx RIx | Eqri RIx Int RIx | Eqrr RIx RIx RIx
        deriving (Eq, Show)

toOp :: RawOp -> Op
toOp (0x0, a, b, c) = Addr (toEnum a) (toEnum b) (toEnum c)
toOp (0x1, a, b, c) = Addi (toEnum a) b          (toEnum c)
toOp (0x2, a, b, c) = Mulr (toEnum a) (toEnum b) (toEnum c)
toOp (0x3, a, b, c) = Muli (toEnum a) b          (toEnum c)
toOp (0x4, a, b, c) = Banr (toEnum a) (toEnum b) (toEnum c)
toOp (0x5, a, b, c) = Bani (toEnum a) b          (toEnum c)
toOp (0x6, a, b, c) = Borr (toEnum a) (toEnum b) (toEnum c)
toOp (0x7, a, b, c) = Bori (toEnum a) b          (toEnum c)
toOp (0x8, a, _, c) = Setr (toEnum a)            (toEnum c)
toOp (0x9, a, _, c) = Seti a                     (toEnum c)
toOp (0xA, a, b, c) = Gtir a          (toEnum b) (toEnum c)
toOp (0xB, a, b, c) = Gtri (toEnum a) b          (toEnum c)
toOp (0xC, a, b, c) = Gtrr (toEnum a) (toEnum b) (toEnum c)
toOp (0xD, a, b, c) = Eqir a          (toEnum b) (toEnum c)
toOp (0xE, a, b, c) = Eqri (toEnum a) b          (toEnum c)
toOp (0xF, a, b, c) = Eqrr (toEnum a) (toEnum b) (toEnum c)
toOp _ = error "bad opcode"

binrr :: (Int -> Int -> Int) -> Regs -> RIx -> RIx -> RIx -> Regs
binrr f r a b c = set r c (f (at r a) (at r b))

binri :: (Int -> Int -> Int) -> Regs -> RIx -> Int -> RIx -> Regs
binri f r a b c = set r c (f (at r a) b)

exec :: Regs -> Op -> Regs
exec r (Addr a b c) = binrr (+)   r a b c
exec r (Addi a b c) = binri (+)   r a b c
exec r (Mulr a b c) = binrr (*)   r a b c
exec r (Muli a b c) = binri (*)   r a b c
exec r (Banr a b c) = binrr (.&.) r a b c
exec r (Bani a b c) = binri (.&.) r a b c
exec r (Borr a b c) = binrr (.|.) r a b c
exec r (Bori a b c) = binri (.|.) r a b c
exec r (Setr a c)   = set r c (at r a)
exec r (Seti a c)   = set r c a
exec r (Gtir a b c) = set r c (fromEnum $ a      > at r b)
exec r (Gtri a b c) = set r c (fromEnum $ at r a > b)
exec r (Gtrr a b c) = set r c (fromEnum $ at r a > at r b)
exec r (Eqir a b c) = set r c (fromEnum $ a      == at r b)
exec r (Eqri a b c) = set r c (fromEnum $ at r a == b)
exec r (Eqrr a b c) = set r c (fromEnum $ at r a == at r b)

at :: Regs -> RIx -> Int
at (R x _ _ _) R0 = x
at (R _ x _ _) R1 = x
at (R _ _ x _) R2 = x
at (R _ _ _ x) R3 = x

set :: Regs -> RIx -> Int -> Regs
set (R _ b c d) R0 x = (R x b c d)
set (R a _ c d) R1 x = (R a x c d)
set (R a b _ d) R2 x = (R a b x d)
set (R a b c _) R3 x = (R a b c x)

toRawOp :: [Int] -> RawOp
toRawOp [a,b,c,d] = (a,b,c,d)
toRawOp _ = error "parse error"

toRegs :: [Int] -> Regs
toRegs [a,b,c,d] = R a b c d
toRegs _ = error "parse error"

shift :: [Int] -> Sample -> Sample
shift xs (x, (a, b, c, d), y) = (x, (xs !! a, b, c, d), y)

shiftN :: RawOp -> Int -> RawOp
shiftN (_,b,c,d) n = (n,b,c,d)

shiftSample :: Sample -> Int -> Sample
shiftSample (before, raw, after) n = (before, shiftN raw n, after)

valid :: Sample -> Bool
valid (before, raw, after) = exec before (toOp raw) == after

parse :: String -> ([Sample], [RawOp])
parse s = (tests , prog)
    where
        [a,b]   = splitOn "\n\n\n\n" s
        triples = splitOn "\n\n" a
        parseSample s = (inp, ops, out)
            where
                [x,y,z] = lines s
                [_, before] = splitOn ": " x
                [_, after]  = splitOn ": " z
                inp = toRegs $ read before
                out = toRegs $ read after
                ops = toRawOp . map read . words $ y
        tests = map parseSample triples
        prog  = map (toRawOp . map read . words) (lines b)

-- number of samples that behave like 3 or more opcodes
part1 :: [Sample] -> Int
part1 ss = length $ filter id $ map has3 ss
    where
        has3 sample = testShift sample 0 [0x0..0xF]
        testShift sample count [] = count >= 3
        testShift sample count (x:xs)
            | count >= 3 = True
            | otherwise = testShift sample (count+m) xs
            where m = fromEnum $ valid (shiftSample sample x)

f :: [Sample] -> [Int]
f ss = head $
    [ p | p <- permutations [0x0..0xF]
    , let ss' = map (shift p) ss
    , and $ map valid ss' ]

part2 :: [Sample] -> [RawOp] -> Int
part2 = undefined

run :: String -> IO ()
run path = do
    (tests, prog) <- parse <$> readFile path
    print tests
    print prog
    print $ map valid tests
    putStrLn $ "Part 1: " ++ (show $ part1 tests)
    print $ f tests
