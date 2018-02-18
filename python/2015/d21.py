weapons = [(8, 4), (10, 5), (25, 6), (40, 7), (74, 8)]
armor = [(13, 1), (31, 2), (53, 3), (75, 4), (102, 5)]
rings = [(25, 1, 0), (50, 2, 0), (100, 3, 0), (20, 0, 1), (40, 0, 2), (80, 0, 3)]


def fight(boss, weapon, armor, rings):
    player_damage = weapon[1] + sum([x[1] for x in rings])
    player_defence = armor[1] + sum([x[2] for x in rings])
    p = [[100, player_damage, player_defence], boss[:]]

    hp, damage, defence = 0, 1, 2

    turn = 0
    while p[0][hp] > 0 and p[1][hp] > 0:
        cur, other = turn % 2, (turn+1) % 2
        inflicted = p[cur][damage] - p[other][defence]
        if inflicted < 1:
            inflicted = 1
        p[other][hp] -= inflicted
        turn += 1
    winner = (turn+1) % 2

    cost = weapon[0] + armor[0] + sum([x[0] for x in rings])
    return winner == 0, cost

def check_win(best, r):
    won, cost = r
    if won:
        if cost < best:
            return cost
    return best

def check_loss(worst, r):
    won, cost = r
    if not won:
        if cost > worst:
            return cost
    return worst

def check(best, worst, r):
    return check_win(best, r), check_loss(worst, r)

def search(boss):
    results = []
    best = 1000
    worst = 0

    for weapon in weapons:
        for arm in [(0,0)] + armor:
            # 0 rings
            best, worst = check(best, worst, fight(boss, weapon, arm, []))

            # 1 ring
            for r in rings:
                best, worst = check(best, worst, fight(boss, weapon, arm, [r]))

            # 2 rings
            for r1 in range(len(rings)-1):
                for r2 in range(r1, len(rings)):
                    best, worst = check(best, worst, fight(boss, weapon, arm, [rings[r1], rings[r2]]))
    return best, worst

boss = [104, 8, 1]
best, worst = search(boss)
print 'Part 1:', best
print 'Part 2:', worst
