from collections import defaultdict
import re
import sys
from itertools import permutations

def parse(line):
    m = re.search(r'(\w+) would (gain|lose) (\d+).*to (\w+).', line)
    who, sign, units, other = m.groups()
    units = int(units) * (-1 if sign == 'lose' else 1)
    return who, units, other

def load():
    g = defaultdict(dict)
    with open(sys.argv[1], 'r') as f:
        for line in f.read().splitlines():
            a, n, b = parse(line)
            g[a][b] = n
    return g

def happiness(x, g):
    h = 0
    for i in range(len(x)):
        p = x[i]
        l = x[(i-1)%len(x)]
        r = x[(i+1)%len(x)]
        h += g[p][l] + g[p][r]
    return h

def naive(g):
    best = None, 0
    memo = {}
    n = 0
    keys = g.keys()
    initial = keys[0]
    for p in permutations(keys):
        pivot = p.index(initial)
        k = (initial,)+p[pivot+1:]+p[:pivot]
        if not memo.has_key(k):
            h = happiness(p, g)
            if h > best[1]:
                best = p, h
            memo[k] = h
            n += 1
    return best

def add_self(g):
    for k in g.keys():
        g['self'][k] = 0
        g[k]['self'] = 0
    return g

g = load()
print 'Part 1:', naive(g)[1]
print 'Part 2:', naive(add_self(g))[1]
