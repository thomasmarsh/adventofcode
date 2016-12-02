def up(p):    return p if p < 3        else p - 3
def down(p):  return p if p > 5        else p + 3
def left(p):  return p if p % 3 == 0   else p - 1
def right(p): return p if (p-2)%3 == 0 else p + 1

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
