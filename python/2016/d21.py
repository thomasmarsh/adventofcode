import sys
import re

def swap_pos(p, a, b):
    p = list(p)
    tmp = p[a]
    p[a] = p[b]
    p[b] = tmp
    return ''.join(p)

def i_swap_pos(p, a, b):
    return swap_pos(p, b, a)

def findall(s, c):
    l = []
    i = s.find(c)
    while i != -1:
        l.append(i)
        i = s.find(c, i+1)
    return l

def swap_letter(p, a, b):
    ai = findall(p, a)
    bi = findall(p, b)
    p = list(p)
    for i in ai:
        p[i] = b
    for i in bi:
        p[i] = a
    return ''.join(p)

def i_swap_letter(p, a, b):
    return swap_letter(p, b, a)

def rotate(p, n):
    l = len(p)
    r = [None]*l
    for i in range(l):
        r[(i+n) % l] = p[i]
    return ''.join(r)

def i_rotate(p, n):
    return rotate(p, -n)

def rotate_letter(p, c):
    i = p.find(c) + 1
    if i > 4:
        i += 1
    return rotate(p, i)

def i_rotate_letter(p, c):
    for i in range(1, len(p) + 1):
        v = rotate(p, -i)
        if rotate_letter(v, c) == p:
            return v

def reverse(p, x, y):
    return p[:x] + ''.join(list(reversed(p[x:y+1]))) + p[y+1:]

def move(p, x, y):
    c = p[x]
    p = p[:x] + p[x+1:]
    return p[:y] + c + p[y:]

def i_move(p, x, y):
    return move(p, y, x)

PROG = [
    (r'swap position (\d+) with position (\d+)',
     swap_pos, i_swap_pos,
     lambda m: (int(m.group(1)), int(m.group(2)))),
    (r'swap letter (\w) with letter (\w)',
     swap_letter, i_swap_letter,
     lambda m: (m.group(1), m.group(2))),
    (r'rotate left (\d+) step',
     rotate, i_rotate,
     lambda m: (-int(m.group(1)),)),
    (r'rotate right (\d+) step',
     rotate, i_rotate,
     lambda m: (int(m.group(1)),)),
    (r'rotate based on position of letter (\w)',
     rotate_letter, i_rotate_letter,
     lambda m: (m.group(1),)),
    (r'reverse positions (\d+) through (\d+)',
     reverse, reverse,
     lambda m: (int(m.group(1)), int(m.group(2)))),
    (r'move position (\d+) to position (\d+)',
     move, i_move,
     lambda m: (int(m.group(1)), int(m.group(2)))),
]

def parse_line(line):
    for p in PROG:
        regex, fn, ifn, args = p
        m = re.search(regex, line)
        if m:
            return fn, ifn, args(m)
    print 'cannot parse:', line
    sys.exit(-1)

def process(lines, p, invert=False):
    for line in lines:
        fn, ifn, args = parse_line(line)
        args = (p,) + args
        f = fn
        if invert:
            f = ifn
        p = f(*args)
    return p

def load():
    with open(sys.argv[1], 'r') as f:
        lines = f.read().splitlines()
    return lines

lines = load()
print 'Part 1:', process(lines, 'abcdefgh')
print 'Part 2:', process(reversed(lines), 'fbgdceah', True)
