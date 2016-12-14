from collections import deque
import math

def f(x, y):
    return x*x + 3*x + 2*x*y + y + y*y

def fp(x, y, n):
    return f(x, y) + n

def popcount(n):
    return bin(n).count('1')

def is_wall(x, y, n):
    return popcount(fp(x, y, n)) % 2 != 0

def p((w, h), n, path=None):
    for y in range(h):
        for x in range(w):
            if path and (x,y) in path:
                print 'O',
            else:
                print '#' if is_wall(x, y, n) else '.',
        print

def graph((w, h), n):
    g = {}
    for y in range(h):
        for x in range(w):
            if not is_wall(x, y, n):
                buf = []
                if x > 0: buf.append((x-1, y, n))
                if y > 0: buf.append((x, y-1, n))
                if x < (w-1): buf.append((x+1, y, n))
                if y < (h-1): buf.append((x, y+1, n))
                g[(x,y)] = [tuple(p[:-1]) for p in buf if not is_wall(*p)]
    return g

def bfs(g, start, end):
    seen = set()
    q = deque([(start, 0, [])])
    while q:
        current, count, path = q.popleft()
        if current == end:
            return count, path + [current]
        path = path[:]
        path.append(current)
        for p in g[current]:
            if not p in seen:
                seen.add(p)
                q.append((p, count+1, path))

def under_50(g, (w, h), a, n):
    c = 0
    for y in range(w):
        for x in range(h):
            result = bfs(g, a, (x, y))
            if result and result[0] <= 50:
                c += 1
    return c

def run(dim, a, b, n):
    assert(not is_wall(a[0], a[1], n))
    assert(not is_wall(b[0], b[1], n))
    g = graph(dim, n)
    result = bfs(g, a, b)
    if not result:
        print 'No path'
    else:
        count, path = result
        p(dim, n, path)
        print count

    print 'Part 2:', under_50(g, dim, a, n)

run((10, 7), (1, 1), (7, 4), 10)

print
print '--'
print

run((33, 42), (1, 1), (31, 39), 1362)
