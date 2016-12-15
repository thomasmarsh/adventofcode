import re
import sys

def parse(lines):
    return [(int(re.search(r'has (\d+)', line).group(1)),
             int(re.search(r'position (\d+)', line).group(1)))
            for line in lines]

def load():
    with open(sys.argv[1], 'r') as f:
        lines = f.read().splitlines()
    return parse(lines)

def run(cfg, t):
    i = 1
    for disc in cfg:
        count, tzero = disc
        position = (tzero + t + i) % count
        if position != 0:
            return None
        i += 1
    return t

def find(cfg):
    t = 0
    while True:
        result = run(cfg, t)
        if result != None:
            return t
        t += 1

c = load()
print 'Part 1:', find(c)
print 'Part 2:', find(c + [(11,0)])
