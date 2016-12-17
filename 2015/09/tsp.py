import sys
import re
from collections import defaultdict, deque
from itertools import permutations

def parse(line, g):
    (a, b, d) = re.search(r'(\w+) to (\w+) = (\d+)', line).groups()
    g[a][b] = int(d)
    g[b][a] = int(d)
    return g

def load():
    with open(sys.argv[1], 'r') as f:
        g = defaultdict(dict)
        return [parse(line, g) for line in f.read().splitlines()][-1]

def distance(g, path):
    p = deque(path)
    d = 0
    while True:
        a = p.popleft()
        d += g[a][p[0]]
        if len(p) == 1:
            break
    return d

def brute(g, shortest=True):
    seen = set()
    cities = g.keys()
    best = None, sys.maxint if shortest else 0
    for p in permutations(cities):
        t = tuple(p)
        if t not in seen:
            seen.add(t)
            seen.add(reversed(t))
            d = distance(g, t)
            if shortest:
                if d < best[1]:
                    best = t, d
            else:
                if d > best[1]:
                    best = t, d
    return best

g = load()

print 'Part 1:', brute(g)[-1]
print 'Part 2:', brute(g, False)[-1]
