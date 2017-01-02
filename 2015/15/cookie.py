import sys
import re

def score(props, m):
    assert(len(props) == len(m))
    x = [0]*(len(props[0]))
    for i in range(len(props)):
        for j in range(len(props[i])):
            x[j] += m[i] * props[i][j]
    return reduce(lambda a,b: 0 if b < 0 else a*b, x[:-1], 1), x[-1]

test = [
    (-1, -2, 6, 3, 8),
    (2, 3, -2, -1, 3),
]
assert(score(test, [44, 56])[0] == 62842880)
assert(score(test, [40, 60])[1] == 500)

MAX = 100

def d(n, m=0, l=[], s=0):
    assert(n >= 1)
    if n == 1:
        assert(sum(l) == s)
        x = l[:] + [MAX-sum(l)]
        assert(sum(x) == MAX)
        return [[MAX-sum(l)]]
    else:
        w = []
        for i in range(0, MAX+1-sum(l)):
            x = l[:] + [i]
            for r in d(n-1, m+1, x, i+s):
                w.append([i]+r)
        return w

def find_numbers(s):
    return re.findall(r'([+-]?\d+)', s)

def load():
    with open(sys.argv[1], 'r') as f:
        lines = f.read().splitlines()
        return [[int(n) for n in find_numbers(line)] for line in lines]

def search():
    data = load()
    best = 0, None
    for entry in d(len(data)):
        s = score(data, entry)[0]
        if s > best[0]:
            best = s, entry
    return best

def search500():
    data = load()
    best = 0, None
    for entry in d(len(data)):
        s, cal = score(data, entry)
        if cal == 500 and s > best[0]:
            best = s, entry
    return best

print 'Part 1:', search()[0]
print 'Part 2:', search500()[0]
