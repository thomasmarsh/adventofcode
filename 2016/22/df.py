import sys

# Filesystem              Size  Used  Avail  Use%
# /dev/grid/node-x0-y0     89T   65T    24T   73%

def parse(data):
    g = {}
    mx, my = 0, 0
    for entry in data:
        x, y = [int(k[1:]) for k in entry[0].split('/')[3].split('-')[1:]]
        node = tuple([int(k) for k in [e[:-1] for e in entry[1:4]]])
        if x > mx: mx = x
        if y > my: my = y
        g[(x,y)] = node
    return g, (mx, my)

def load():
    with open(sys.argv[1], 'r') as f:
        data = [line.split() for line in f.read().splitlines()[2:]]
    return parse(data)

def viable(g):
    count = 0
    ks = g.keys()
    for k1 in ks:
        for k2 in ks:
            if k1 != k2:
                _, n1_used, _ = g[k1]
                _, _, n2_avail = g[k2]
                if n1_used > 0 and n1_used < n2_avail:
                    count += 1
    return count

def p(g, (mx, my)):
    wall = g[(0,0)][0]
    for y in range(my+1):
        for x in range(mx+1):
            size, used, avail = g[(x,y)]
            if (x,y) == (0,0):
                print '=',
            elif (x,y) == (mx, 0):
                print 'G',
            elif used == 0:
                print '_',
            elif used >= wall:
                print '#',
            else:
                print '.',
        print

def find_empty(g, (mx, my)):
    for y in range(my+1):
        for x in range(mx+1):
            if g[(x, y)][1] == 0:
                return x, y
    return None

def distance(g, (mx, my)):
    x0, y0 = find_empty(g, (mx, my))
    return x0-2+y0+mx+(mx-1)*5 

g, (mx, my) = load()
print 'Part 1:', viable(g)
print 'Part 2:',  distance(g, (mx, my))
# Print the map
#p(g, (mx, my))

