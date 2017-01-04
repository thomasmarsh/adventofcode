import sys

def load():
    with open(sys.argv[1] if len(sys.argv) > 1 else 'input', 'r') as f:
        lines = f.read().splitlines()
    return [line.split() for line in lines]

def run(prog, initial_a=0):
    pc = b = 0
    a = initial_a
    while pc < len(prog):
        x = prog[pc]
        pc += 1
        if x[0] == 'hlf': exec('%s /= 2' % x[1])
        elif x[0] == 'tpl': exec('%s *= 3' % x[1])
        elif x[0] == 'inc': exec('%s += 1' % x[1])
        elif x[0] == 'jmp': pc += int(x[1])-1
        elif x[0] == 'jie':
            if eval(x[1])[0] % 2 == 0:
                pc += int(x[2])-1
        elif x[0] == 'jio':
            if eval(x[1])[0] == 1:
                pc += int(x[2])-1
    return a, b

prog = load()
print 'Part 1:', run(prog)[1]
print 'Part 2:', run(prog, 1)[1]
