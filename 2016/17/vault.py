from hashlib import md5
from collections import deque

pos = (0,0)

def unlocked(k):
    return [c in 'bcdef' for c in md5(k).hexdigest()[:4]]


def valid((x,y)):
    return x >= 0 and x < 4 \
       and y >= 0 and y < 4

def add(a, b):
    return (a[0]+b[0], a[1]+b[1])

def moves(state):
    pos, code = state
    if pos == (3,3): return []
    udlr = zip(unlocked(code),
               'UDLR',
               [(0, -1), (0, 1), (-1, 0), (1, 0)])
    return [(y[1], add(y[2], pos)) for y in 
            filter(lambda x: x[0] and valid(add(pos, x[2])), udlr)]

def search(code, longest=False):
    q = deque([((0,0), code)])
    best = ''
    while q:
        pos, current = q.popleft()
        for letter, new_pos in moves((pos, current)):
            new_code = current+letter
            state = (new_pos, new_code)
            if new_pos == (3,3):
                if not longest:
                    return current[len(code):]+letter
                if len(new_code) > len(best):
                    best = new_code
            q.append(state)
    return best[len(code):]

assert(search('ihgpwlah') == 'DDRRRD')
assert(search('kglvqrro') == 'DDUDRLRRUDRD')
assert(search('ulqzkmiv') == 'DRURDRUDDLLDLUURRDULRLDUUDDDRR')

print 'Part 1:', search('pvhmgsws')
print 'Part 2:', len(search('pvhmgsws', True))
