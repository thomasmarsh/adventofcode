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

def d(n, high=100, m=0, l=[], s=0):
    assert(n >= 1)
    if n == 1:
        x = l[:] + [high-sum(l)]
        return [[high-sum(l)]]
    else:
        w = []
        for i in range(0, high+1-sum(l)):
            x = l[:] + [i]
            for r in d(n-1, high, m+1, x, i+s):
                w.append([i]+r)
        return w

def load():
    with open(sys.argv[1] if len(sys.argv) > 1 else 'input', 'r') as f:
        lines = f.read().splitlines()
        return [[int(n) for n in re.findall(r'[-]?\d+', line)] for line in lines]

def search():
    data = load()
    best = 0
    cal_best = 0
    for entry in d(len(data)):
        s, cal = score(data, entry)
        if s > best:
            best = s
        if cal == 500 and s > cal_best:
            cal_best = s
    return best, cal_best

part1, part2 = search()
print 'Part 1:', part1
print 'Part 2:', part2
