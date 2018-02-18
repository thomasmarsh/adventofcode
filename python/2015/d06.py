import sys
import re

def lit(state):
    return sum(map(lambda x: sum(x), state))

def apply(state, mode, (fn, (x1, y1, x2, y2))):
    for x in range(x1, x2+1):
        for y in range(y1, y2+1):
            state[y][x] = fn(state[y][x], mode)
    return state

def toggle(n, mode):
    if mode == 0:
        return 1-n
    return n+2

def on(n, mode):
    if mode == 0:
        return 1
    return n+1

def off(n, mode):
    if mode == 0:
        return 0
    if n > 0:
        return n - 1
    return 0

def load():
    with open(sys.argv[1], 'r') as f:
        lines = f.read().splitlines()
    p = re.compile(r'(\d+),(\d+) through (\d+),(\d+)')
    cmds = []
    fn_map = { 'turn on': on, 'turn off': off, 'toggle': toggle }
    for line in lines:
        r = [int(x) for x in p.search(line).groups()]
        for k, fn in fn_map.items():
            if line.startswith(k):
                cmds.append((fn, r))
                break
    return cmds

def run(data, mode):
    state = [[0] * 1000 for _ in range(1000)]
    for cmd in data:
        state = apply(state, mode, cmd)
    return lit(state)

data = load()
print 'Part 1:', run(data, 0)
print 'Part 2:', run(data, 1)
