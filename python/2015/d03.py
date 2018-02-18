import sys

def deliver(data, houses):
    fmap = { '^': lambda (x,y): (x, y-1),
             'v': lambda (x,y): (x, y+1),
             '<': lambda (x,y): (x-1, y),
             '>': lambda (x,y): (x+1, y) }

    p = (0,0)
    houses.add(p)
    for c in data:
        p = fmap[c](p)
        houses.add(p)
    return houses

def deliver2(data):
    houses = deliver(data[:][::2], set())
    return deliver(data[1:][::2], houses)

assert(len(deliver2('^v')) == 3)
assert(len(deliver2('^>v<')) == 3)
assert(len(deliver2('^v^v^v^v^v')) == 11)

def main():
    with open(sys.argv[1], 'r') as f:
        data = f.read().strip()

    print 'Part 1:', len(deliver(data, set()))

    # Part 2
    print 'Part 2:', len(deliver2(data))

main()
