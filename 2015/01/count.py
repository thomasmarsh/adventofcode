import sys

def count(data):
    inc = data.count('(')
    dec = data.count(')')
    return inc-dec

with open(sys.argv[1], 'r') as f:
    data = f.read()
    print 'Part 1:', count(data)

    i = 0
    while i < len(data):
        c = count(data[:i])
        if c < 0:
            print 'Part 2:', i
            break
        i += 1
