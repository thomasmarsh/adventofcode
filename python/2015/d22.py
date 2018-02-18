import sys

SPELLS = [
    # cost, mana, opponent_hp, self_hp, armor, duration
    ( 53,   0,    4,           0,       0,     1), # Magic missile
    ( 73,   0,    2,           2,       0,     1), # Drain
    ( 113,  0,    0,           0,       7,     6), # Shield
    ( 173,  0,    3,           0,       0,     6), # Poison
    ( 229,  101,  0,           0,       0,     5), # Recharge
]

def moves(mana, eff):
    m = []
    for i in range(len(eff)):
        if eff[i] == 0 and mana > SPELLS[i][0]:
            m.append(i)
    return m

def apply_spell(spell, hp, mana, armor, boss_hp):
    return (hp + spell[3],
            mana + spell[1],
            armor + spell[4],
            boss_hp - spell[2])

def apply_effects(eff, hp, mana, boss_hp):
    armor = 0
    for i in range(len(eff)):
        if eff[i] > 0:
            hp, mana, armor, boss_hp = apply_spell(SPELLS[i], hp, mana, armor, boss_hp)
    return hp, mana, armor, boss_hp

def search(eff,
           (boss_hp, boss_damage),
           (hp, mana),
           cost=0,
           best_cost=sys.maxint,
           turn=True,
           seen={},
           part2=False):

    k = (turn, hp, mana, boss_hp, tuple(eff))
    if k in seen:
        return seen[k]

    def memo(a, b):
        seen[k] = a, b
        return a, b

    if part2:
        hp -= 1

    # Apply effects
    hp, mana, armor, boss_hp = apply_effects(eff, hp, mana, boss_hp)
    if boss_hp <= 0:
        return memo(True, cost)


    # Decrement timers
    eff = [x-1 if x > 0 else x for x in eff]

    # Boss turn
    if not turn:
        damage = boss_damage - armor
        hp -= damage if damage > 0 else 1
        if hp <= 0:
            return memo(False, None)

        return search(eff,
                     (boss_hp, boss_damage),
                     (hp, mana),
                     cost,
                     best_cost,
                     True,
                     seen,
                     part2)

    # Player turn
    else:
        ms = moves(mana, eff)
        if len(ms) == 0:
            return memo(False, None)

        best = False, best_cost

        for m in ms:
            spell_cost, duration = SPELLS[m][0], SPELLS[m][-1]

            ncost = cost+spell_cost
            if ncost < best[1]:
                xeff = eff[:]
                xeff[m] = duration
                r = search(xeff,
                           (boss_hp, boss_damage),
                           (hp, mana-spell_cost),
                           ncost,
                           best[1],
                           False,
                           seen,
                           part2)
                if r[0] and (not r[0] or r[1] < best[1]):
                    best = r

        return memo(*best)

player = (50, 500) # hp, mana
boss = (58, 9) # hp, damage
effects = [0]*len(SPELLS)

print 'Part 1:', search(effects, boss, player, seen={})[1]
print 'Part 2:', search(effects, boss, player, seen={}, part2=True)[1]
