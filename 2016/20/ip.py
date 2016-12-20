import sys

def solve(path):
    data.sort()
    n, count = 0, 0
    lowest = None
    for lo, hi in data:
        if n < lo:
            if lowest == None:
                lowest = n
            count += lo - n
        if n <= hi:
            n = hi + 1
    count += 2 ** 32 - n
    return lowest, count

def load():
    with open(sys.argv[1], 'r') as f:
        lines = f.read().splitlines()
    return [map(int, x.split('-')) for x in lines]

data = load()
lowest, count = solve(data)
print 'Part 1:', lowest
print 'Part 2:', count
