def up(p):
    if p < 3: return p
    return p - 3

def down(p):
    if p > 5: return p
    return p + 3

def left(p):
    if p % 3 == 0: return p
    return p - 1

def right(p):
    if (p-2) % 3 == 0: return p
    return p + 1

lut = { 'U': up, 'D': down, 'L': left, 'R': right }

def move(p, d):
    return lut[d](p)

with open('input') as f:
    data = map(lambda x: x.strip(), f.readlines())

p = 4
code = ''
for line in data:
    p = reduce(move, line, p)
    code += str(p+1)
print code
