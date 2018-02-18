import sys

def area(l, w, h):
    return 2*l*w + 2*w*h + 2*h*l

def smallest(l, w, h):
    return sorted((l, w, h))[:2]

def paper(l, w, h):
    x = smallest(l, w, h)
    return area(l, w, h) + x[0] * x[1]

def ribbon(l, w, h):
    x = smallest(l, w, h)
    perim = x[0]*2 + x[1]*2
    bow = l * w * h
    return perim + bow


with open(sys.argv[1], 'r') as f:
    lines = f.read().splitlines()
    total = 0
    for line in lines:
        l, w, h = [int(x) for x in line.split('x')]
        total += paper(l, w, h)
    print 'Part 1:', total
    total = 0
    for line in lines:
        l, w, h = [int(x) for x in line.split('x')]
        total += ribbon(l, w, h)
    print 'Part 2:', total
