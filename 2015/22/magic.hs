import           Data.Foldable (toList)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Data.Sequence as S

data Spell = Spell {
    spCost       :: Int,
    spMana       :: Int,
    spOpponentHp :: Int,
    spSelfHp     :: Int,
    spArmor      :: Int,
    spDuration   :: Int
} deriving (Show)

data State = State {
    stHp         :: Int,
    stMana       :: Int,
    stArmor      :: Int,
    stBossHp     :: Int,
    stEffects    :: [Int]
} deriving (Show)

data SearchState = SearchState {
    ssState    :: State,
    ssCost     :: Int,
    ssBestCost :: Int,
    ssSeen     :: Memo,
    ssTurn     :: Bool,
    ssIsHard   :: Bool
} deriving (Show)

type MemoKey = (Bool, Int, Int, Int, [Int])

type Memo = M.Map MemoKey (Bool, Int)

type InternalSearchResult = (Bool, Int, Memo)

genSpell :: (Int, Int, Int, Int, Int, Int) -> Spell
genSpell (cost, mana, opponentHp, selfHp, armor, duration) =
    Spell { spCost       = cost,
            spMana       = mana,
            spOpponentHp = opponentHp,
            spSelfHp     = selfHp,
            spArmor      = armor,
            spDuration   = duration }

spells :: [Spell]
spells = map genSpell [
  -- cost, mana, opponent_hp, self_hp, armor, duration
    (53,   0,    4,           0,       0,     1), -- Magic missile
    (73,   0,    2,           2,       0,     1), -- Drain
    (113,  0,    0,           0,       7,     6), -- Shield
    (173,  0,    3,           0,       0,     6), -- Poison
    (229,  101,  0,           0,       0,     5)] -- Recharge

initialState :: State
initialState = State {
    stHp      = 50,
    stMana    = 500,
    stArmor   = 0,
    stBossHp  = 58,
    stEffects = replicate (length spells) 0
}

bossDamage :: Int
bossDamage = 9

applySpell :: State -> Spell -> State
applySpell state spell =
    state { stHp     = stHp state     + spSelfHp spell,
            stMana   = stMana state   + spMana spell,
            stArmor  = stArmor state  + spArmor spell,
            stBossHp = stBossHp state - spOpponentHp spell }

moves :: State -> [Int]
moves state = [i | (n, i) <- zip (stEffects state) [0..]
                 , n == 0
                 , stMana state >= spCost (spells !! i)]

startEffect :: State -> Int -> Int -> Int -> State
startEffect state i cost duration =
    state { stEffects = toList (S.update i duration
                                   (S.fromList (stEffects state))),
            stMana = stMana state - cost}

applyEffects :: State -> State
applyEffects state = foldl applySpell resetArmor effects
    where resetArmor = state { stArmor = 0 }
          effects = [spell | (spell, n) <- zip spells (stEffects state), n > 0]

decayEffects :: State -> State
decayEffects state = state {
    stEffects = [max 0 (n-1) | n <- stEffects state]
}
    
memoKey :: Bool -> State -> MemoKey
memoKey turn state = (turn, stHp state, stMana state, stBossHp state, stEffects state)

memoize :: Bool -> SearchState -> (Bool, Int, Memo)
memoize won ss = (won, cost, inserted)
    where cost = ssCost ss
          key = memoKey (ssTurn ss) (ssState ss)
          inserted = M.insert key (won, cost) (ssSeen ss)

flipTurn :: SearchState -> SearchState
flipTurn ss = ss { ssTurn = not (ssTurn ss) }

bossTurn :: SearchState -> InternalSearchResult
bossTurn ss
    | stHp reduced <= 0 = memoize False ss
    | otherwise = search' (flipTurn (ss { ssState = reduced }))
    where state = ssState ss
          rawDamage = bossDamage - stArmor state
          damage | rawDamage < 1 = 1 | otherwise = rawDamage
          reduced = state { stHp = stHp state - damage }

betterThan :: InternalSearchResult -> InternalSearchResult -> InternalSearchResult
betterThan a@(_, sa, _) b@(won, sb, _)
    | won && sb < sa = b
    | otherwise = a

foldEff :: InternalSearchResult -> [(SearchState, Int)] -> InternalSearchResult
foldEff current [] = current
foldEff current ((ss, cost):xs) = foldEff newBest xs
    where (_, bestScore, _) = current
          newBest :: InternalSearchResult
          newBest
            | cost < bestScore =
                 current `betterThan`
                     search' (ss { ssCost = cost, ssBestCost = bestScore })
            | otherwise = current

playerTurn :: SearchState -> InternalSearchResult
playerTurn ss
    | null availSpells = memoize False ss
    | otherwise = memoize rwin rss
    where state = ssState ss
          availSpells = [(i, spells !! i) | i <- moves state]
          costs1 = [spCost spell | (_, spell) <- availSpells]
          costs2 = [ssCost ss + spCost spell | (_, spell) <- availSpells]
          newEffects = [flipTurn (ss { ssState = startEffect state i cost (spDuration spell) })
                            | ((i, spell), cost) <- zip availSpells costs1]
          (rwin, rcost, rseen) = foldEff (False, ssBestCost ss, ssSeen ss) (zip newEffects costs2)
          rss = ss { ssCost = rcost,
                     ssSeen = rseen }

search' :: SearchState -> InternalSearchResult
search' ss
    | M.member key seen = (won', cost', seen)
    | stBossHp state' <= 0 = memoize True ss
    | otherwise = next ss'
    where state = ssState ss
          seen = ssSeen ss
          key = memoKey (ssTurn ss) state
          (won', cost') = fromJust (M.lookup key seen)
          decrementHp | ssIsHard ss = state { stHp = stHp state - 1 } | otherwise = state
          state' = (decayEffects . applyEffects) decrementHp
          ss' = ss { ssState = state' }
          next | ssTurn ss = playerTurn | otherwise = bossTurn

search :: Bool -> Int
search isHard = cost
    where (_, cost, _) = search' SearchState {
            ssIsHard   = isHard,
            ssState    = initialState,
            ssCost     = 0,
            ssBestCost = maxBound :: Int,
            ssTurn     = True,
            ssSeen     = M.empty }

main :: IO ()
main = do
    putStrLn $ "Part 1: " ++ show (search False)
    putStrLn $ "Part 2: " ++ show (search True)
