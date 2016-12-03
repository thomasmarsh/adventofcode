def turn(s, c):
    if   c == 'R': return ((s[0]+1)%4,) + s[1:]
    elif c == 'L': return ((s[0]-1)%4,) + s[1:]
    return s

def fwd(s, n):
    d, x, y = s
    if   d == 0: y -= n
    elif d == 1: x += n
    elif d == 2: y += n
    elif d == 3: x -= n
    return (d, x, y)

def move(s, i):
    n = fwd(turn(s, i[0]), int(i[1:]))
    return n

def part1(data):
    s = reduce(move, data, (0,0,0))
    print 'Part1:', dist(s)

def dist(s): return abs(s[1])+abs(s[2])

def points_between(s, a, b):
    dx = b[0]-a[0]
    dy = b[1]-a[1]
    if dx != 0: dx = dx / abs(dx)
    if dy != 0: dy = dy / abs(dy)
    x, y = a
    while (x, y) != b:
        c = (x, y)
        if c in s:
            return s, c
        s.add(c)
        x += dx
        y += dy
    return s, None

def part2(data):
    s = (0,0,0)
    seen = set()
    result = None
    for y in data:
        x = move(s, y)
        seen, result = points_between(seen, s[1:], x[1:])
        s = x
        if result: break
    print 'Part2:', dist((0,)+result)

def load():
    with open('input') as f:
        return f.read().strip().split(', ')

def main():
    data = load()
    part1(data)
    part2(data)

if __name__ == '__main__': main()
