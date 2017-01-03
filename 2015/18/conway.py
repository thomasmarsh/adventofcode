import sys

def load():
    with open(sys.argv[1] if len(sys.argv) > 1 else 'input') as f:
        return f.read().splitlines()

#A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise.
#A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.

def is_set(state, x, y):
    return x >= 0 and y >= 0 and \
           y < len(state) and x < len(state[y]) and \
           state[y][x] == '#'


def neighbors(state, x, y):
    n = 0
    for ox in range(-1, 2):
        for oy in range(-1, 2):
            if ox or oy:
                n += is_set(state, x+ox, y+oy)
    return n

#A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise.
#A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.

def fixate(state):
    pstate = [list(x) for x in state]
    pstate[0][0] = '#'
    pstate[0][-1] = '#'
    pstate[-1][0] = '#'
    pstate[-1][-1] = '#'
    return pstate

def step(state, fixed=False):
    if fixed:
        state = fixate(state)
    pstate = []
    for i in range(len(state)):
        pstate.append(['.']*len(state[0]))
    for y in range(len(state)):
        for x in range(len(state[y])):
            n = neighbors(state, x, y)
            if state[y][x] == '#':
                if n in (2, 3):
                    pstate[y][x] = '#'
                else:
                    pstate[y][x] = '.'
            else:
                if n == 3:
                    pstate[y][x] = '#'
                else:
                    pstate[y][x] = '.'
    if fixed:
        pstate = fixate(pstate)
    return [''.join(x) for x in pstate]

def run(state, n, fixed=False):
    for i in range(n):
        state = step(state, fixed)
    return ''.join(state).count('#')

state = load()
print 'Part 1:', run(state, 100)
print 'Part 2:', run(state, 100, True)
