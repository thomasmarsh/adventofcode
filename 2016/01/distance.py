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

with open('input') as f:
    data = f.read().strip().split(', ')

s = reduce(lambda x, y: fwd(turn(x, y[0]), int(y[1:])), data, (0,0,0))

print s[1]+s[2]
