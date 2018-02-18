import sys
from itertools import combinations
from operator import mul

def load():
    with open(sys.argv[1] if len(sys.argv) > 1 else 'input', 'r') as f:
        lines = f.read().splitlines()
    return map(int, lines)

def solve(d, n):
    target = sum(d) // n
    for i in range(len(d)):
        q = [reduce(mul, c) for c in combinations(d, i) 
             if sum(c) == target]
        if q:
            return min(q)

d = load()
print(solve(d, 3))
print(solve(d, 4))
