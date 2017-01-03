from itertools import combinations

def load():
    return map(int, open('input', 'r').read().splitlines())

def count(data, filt=None):
    n = 0
    min_len = len(data)
    solutions = []
    for i in range(len(data)):
        m = 0
        for c in combinations(data, i):
            if sum(c) == 150:
                m += 1
                if len(c) < min_len:
                    min_len = len(c)
                solutions.append(c)
        n += m

    min_count = 0
    for s in solutions:
        if len(s) == min_len:
            min_count += 1
    return n, min_count

total, min_len = count(load())
print 'Part 1:', total
print 'Part 2:', min_len
