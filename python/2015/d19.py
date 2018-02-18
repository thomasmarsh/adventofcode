import re
import sys

def load():
    with open(sys.argv[1] if len(sys.argv) > 1 else 'input', 'r') as f:
        lines = f.read().splitlines()
        d = []
        for line in lines:
            w = re.findall('\w+', line)
            if len(w) == 2:
                d.append(w)
            elif len(w) == 1:
                return w[0], d

def count(m, d):
    x = set()
    for k, v in d:
        i = m.find(k)
        while i >= 0:
            x.add(m[:i] + v + m[i+len(k):])
            i = m.find(k, i+1)
    return len(x)

def steps(m, d):
    elts = re.findall('[A-Z][a-z]?', m)
    rs = elts.count('Rn') + elts.count('Ar')
    ys = elts.count('Y')
    return len(elts) - rs - 2 * ys - 1


m, d = load()
print 'Part 1:', count(m, d)
print 'Part 2:', steps(m, d)
