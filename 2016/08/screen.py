# this could use a cleanup

WIDTH = 50
HEIGHT = 6

SCREEN = [0]*WIDTH*HEIGHT

COLUMN = 0
ROW = 1

def show():
    cmap = {0: '.', 1: '#'}
    for y in range(HEIGHT):
        buf = []
        for x in range(WIDTH):
            buf.append(SCREEN[xy_to_i(x, y)])
        print ''.join([cmap[v] for v in buf])

def lrotate(l, n):
    return l[-n:] + l[:-n]

def xy_to_i(x, y):
    return y*WIDTH+ x

def sset(x, y, v=1):
    SCREEN[xy_to_i(x, y)] = v

def rect(_x,_y):
    for x in range(_x):
        for y in range(_y):
            sset(x, y)

def crotate(x, amt):
    d = []
    for y in range(HEIGHT):
        d.append(SCREEN[xy_to_i(x, y)])
    d = lrotate(d, amt)
    for y in range(HEIGHT):
        sset(x, y, d[y])

def rrotate(y, amt):
    d = []
    for x in range(WIDTH):
        d.append(SCREEN[xy_to_i(x, y)])
    d = lrotate(d, amt)
    for x in range(WIDTH):
        sset(x, y, d[x])

def process_line(line):
    parts = line.split()
    cmd = parts[0]
    if cmd == 'rect':
        x,y = [int(v) for v in parts[1].split('x')]
        rect(x, y)
    else:
        assert(cmd == 'rotate')
        i = int(parts[2][2:])
        amt = int(parts[-1])
        if parts[1] == 'row':
            rrotate(i, amt)
        else: crotate(i, amt)

test = """rect 3x2
rotate column x=1 by 1
rotate row y=0 by 4
rotate column x=1 by 1"""

with open('input', 'r') as f:
    for line in f.readlines():
        process_line(line.strip())

show()
print SCREEN.count(1)
