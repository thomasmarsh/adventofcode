import sys

def parse(data):
    g = {}
    for line in data:
        l, r = line.split(' -> ')
        g[r] = l.split()
    return g

def eval2(k, g, memo):
    if k.isdigit():
        return int(k)
    if k not in memo:
        v = g[k]
        if len(v) == 1:
            memo[k] = eval2(v[0], g, memo)
        elif len(v) == 2:
            assert(v[0] == 'NOT')
            memo[k] = ~eval2(v[1], g, memo) & 0xffff
        else:
            assert(len(v) == 3)
            x, op, y = v
            if op == 'AND':
                memo[k] = eval2(x, g, memo) & eval2(y, g, memo)
            elif op == 'OR':
                memo[k] = eval2(x, g, memo) | eval2(y, g, memo)
            elif op == 'LSHIFT':
                memo[k] = eval2(x, g, memo) << int(y)
            elif op == 'RSHIFT':
                memo[k] = eval2(x, g, memo) >> int(y)
            else:
                raise 'Unexpected'
    return memo[k]

def evaluate(g):
    memo = {}
    for k in g.keys():
        eval2(k, g, memo)
    return memo

g = parse(open(sys.argv[1], 'r').read().splitlines())
m = evaluate(g)
print 'Part 1:', m['a']

g['b'] = [str(m['a'])]
m = evaluate(g)
print 'Part 2:', m['a']
