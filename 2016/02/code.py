def load():
    with open('input') as f:
        return map(lambda x: x.strip(), f.readlines())

def part1(data):
    def up(p):    return p if p < 3        else p - 3
    def down(p):  return p if p > 5        else p + 3
    def left(p):  return p if p % 3   == 0 else p - 1
    def right(p): return p if (p-2)%3 == 0 else p + 1

    def move(p, d):
        lut = { 'U': up, 'D': down, 'L': left, 'R': right }
        return lut[d](p)

    p = 4
    code = ''
    for line in data:
        p = reduce(move, line, p)
        code += str(p+1)
    print code

def part2(data):
    valid = 0b0010001110111110111000100
    keypad = '**1***234*56789*ABC***D**'
    #         0123456789012345678901234
    #         0         1         2

    def bad(p):   return (valid & (1 << p)) != (1 << p)
    def up(p):    return p if p < 5        or bad(p-5) else p - 5
    def down(p):  return p if p > 19       or bad(p+5) else p + 5
    def left(p):  return p if p % 5   == 0 or bad(p-1) else p - 1
    def right(p): return p if (p-4)%5 == 0 or bad(p+1) else p + 1

    def move(p, d):
        lut = { 'U': up, 'D': down, 'L': left, 'R': right }
        return lut[d](p)

    for i in range(len(keypad)):
        if keypad[i] == '*': assert(bad(i))
        else: assert(not bad(i))
    p = keypad.index('5')
    code = ''
    for line in data:
        p = reduce(move, line, p)
        code += keypad[p]
    print code

def main():
    data = load()
    part1(data)
    part2(data)

if __name__ == '__main__': main()
