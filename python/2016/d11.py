from collections import defaultdict
from itertools import combinations
from collections import deque
import copy
import sys
import re

G   = 0  # Generator
M   = 1  # Microchip
INC = 1  # Increment E
DEC = -1 # Decrement E

def floor_rep(f):
    def tfmt(x): return 'G' if x == G else 'M'
    return ','.join(['{}{}'.format(x[0], tfmt(x[1])) for x in f])

def pstate(s, prefix='>'):
    e, floors = s
    n = len(floors)
    print prefix, 'Floor:', n-e
    for i in range(n):
        print prefix, n-i, floor_rep(floors[i])

def move_rep(m):
    def dfmt(d): return '+' if d == INC else '-'
    d, s = m
    return '{},{}'.format(dfmt(d), floor_rep(s))

def is_valid_config(floor):
    gs = set([x[0] for x in floor if x[1] == G])
    ms = set([x[0] for x in floor if x[1] == M])
    if not gs:
        return True
    if ms - gs:
        return False
    return True

def perform(s, move):
    e, f = s
    nf = copy.deepcopy(f)
    d, take = move
    nf[e] = nf[e] - take
    ne = e + d
    nf[ne] = nf[ne] | take
    return (ne, nf)

def validate(state):
    e, floors = state
    assert(e >= 0)
    assert(e < 4)
    for floor in floors:
        assert(is_valid_config(floor))

def is_valid_move(state, move):
    e, floors = perform(state, move)
    for floor in floors:
        if not is_valid_config(floor):
            return False
    return True

def combos(items):
    a = [set([x]) for x in items]
    b = [set(x) for x in combinations(items, 2) if is_valid_config(x)]
    return a + b

def dir_moves(state, d):
    e, floors = state
    m = combos(floors[e])
    return [(d, i) for i in m if is_valid_move(state, (d, i))]

def moves(state):
    m = []
    if state[0] < 3: m += dir_moves(state, INC)
    if state[0] > 0: m += dir_moves(state, DEC)
    return m

def end_condition(state):
    e, floors = state
    for floor in floors[1:]:
        if floor:
            return False
    return True

def cache_key(s):
    h = defaultdict(lambda: [None, None])
    for i, floor in enumerate(s[1]):
        for x, t in floor:
            h[x][t] = i
    return s[0], tuple(sorted(map(tuple, h.values())))

def bfs(state):
    d = 0
    q = deque(((state, d),))

    not_found = sys.maxint, d, False
    cache = set()
    last_d = d
    while q:
        current, d = q.popleft()

        x = '[{}] '.format(d) + '*'*d + '>'
        if d != last_d:
            last_d = d
            print x
        pstate(current, x)
        validate(current)

        for m in moves(current):
            s = perform(current, m)
            if end_condition(s):
                return d+1
            k = cache_key(s)
            if not k in cache:
                cache.add(k)
                q.append((s,d+1))

    return not_found


def load():
    matchers = [
        (re.compile(r'(\w+) generator'), G),
        (re.compile(r'(\w+)-compat'), M)
    ]
    if len(sys.argv) < 2:
        print 'usage: elevator.py <input>'
        sys.exit(-1)
    with open(sys.argv[1], 'r') as f:
        lines = f.read().splitlines()

    ids = {}
    i = 0
    floors = []
    lines.reverse()
    for line in lines:
        floors.append(set())
        for prog, t in matchers:
            for match in prog.findall(line):
                if match not in ids:
                    ids[match] = i
                    i += 1
                n = ids[match]
                floors[-1].add((n, t))
    return 3, floors


state = load()
validate(state)
print bfs(state)
