import sys
import re
from collections import deque
from itertools import permutations, izip

def load():
    with open(sys.argv[1], 'r') as f:
        data = f.read().strip()
    targets = re.findall(r'[0-9]+', data)
    targets.sort()
    return data.splitlines(), targets

def find(lines, needle):
    y = 0
    for line in lines:
        x = line.find(needle)
        if x != -1:
            return x, y
        y += 1
    return None

def graph(lines):
    g = {}
    y = 0
    for line in lines:
        x = 0
        for c in line:
            pos = (x,y)
            if c != '#':
                g[pos] = []
                if x > 0:
                    if lines[y][x-1] != '#':
                        g[pos].append((x-1,y))
                if y > 0:
                    if lines[y-1][x] != '#':
                        g[pos].append((x,y-1))
                if x < len(line)-1:
                    if lines[y][x+1] != '#':
                        g[pos].append((x+1,y))
                if y < len(lines)-1:
                    if lines[y+1][x] != '#':
                        g[pos].append((x,y+1))
            x += 1
        y += 1
    return g

def bfs(g, start, end):
    seen = set()
    q = deque([(start, 0, [])])
    while q:
        current, count, path = q.popleft()
        if current == end:
            return count, path + [current]
        path = path[:]
        path.append(current)
        for p in g[current]:
            if not p in seen:
                seen.add(p)
                q.append((p, count+1, path))

def paths(data, targets):
    g = graph(data)
    l = {}
    p = [find(data, x) for x in targets]
    for i in range(0, len(targets)-1):
        for j in range(i+1, len(targets)):
            s = bfs(g, p[i], p[j])[0]
            a, b = targets[i], targets[j]
            l[(a,b)] = s
            l[(b,a)] = s
    return l

def length(p, route):
    n = 0
    for pair in izip(route[:-1], route[1:]):
        n += p[pair]
    return n

def hamilton(data, targets, roundtrip=None):
    best = sys.maxint, None
    p = paths(data, targets)
    if roundtrip:
        del targets[targets.index(roundtrip)]
    for route in permutations(targets):
        if roundtrip:
            route = [roundtrip] + list(route) + [roundtrip]
        l = length(p, route)
        if l < best[0]:
            best = l, route
    return best

data, targets = load()
print 'Part 1:', hamilton(data, targets)[0]
print 'Part 2:', hamilton(data, targets, '0')[0]
