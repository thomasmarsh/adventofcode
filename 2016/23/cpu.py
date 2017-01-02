# Python is definitely not the language for this kind of work... Slow!

from collections import defaultdict
import string
import sys

def is_int(s):
    try: 
        int(s)
        return True
    except ValueError:
        return False

class CPU:
    def __init__(self, prog, r):
        self.pc = 0      # Program counter
        self.prog = prog # Program
        self.r = r       # Registers
        self.verbose = False

    def done(self):
        return self.pc >= len(self.prog)

    def cpy(self, x, y):
        assert(not is_int(y))
        self.r[y] = self.val(x)
        self.pc += 1

    def val(self, x):
        return int(x) if is_int(x) else self.r[x]

    def can_optimize(self):
        # cpy a b
        # inc x     <-- current location
        # dec b
        # jnz b -2
        # dec c
        # jnz c -5

        if self.pc+3 < len(self.prog) and self.pc-1 >= 0:
            instrs = [p[0] for p in self.prog[self.pc-1:self.pc+5]]
            if instrs == ['cpy', 'inc', 'dec', 'jnz', 'dec', 'jnz']:
                return True
        return False

    def mul(self, x):
        i = self.pc-1
        prog = self.prog

        cpy_a, cpy_b     = prog[i][1:]
        dec1_b           = prog[i+2][1]
        jnz_b, jnz_b_off = prog[i+3][1:]
        dec2_c           = prog[i+4][1]
        jnz_c, jnz_c_off = prog[i+5][1:]

        if cpy_b == dec1_b and dec1_b == jnz_b and \
           dec2_c == jnz_c and \
           jnz_b_off == '-2' and jnz_c_off == '-5':
            self.r[x] += self.val(cpy_a) * self.val(dec2_c)
            self.r[dec1_b] = 0
            self.r[dec2_c] = 0
            self.pc += 5
            return True
        return False

    def inc(self, x):
        if x in self.r:
            # Peephole optimize
            if self.can_optimize():
                if self.mul(x):
                    return
        self.r[x] += 1
        self.pc += 1

    def dec(self, x):
        self.r[x] -= 1
        self.pc += 1

    def jnz(self, x, y):
        q, w = [int(z) if is_int(z) else self.r[z] for z in (x,y)]
        if q != 0:
            self.pc = self.pc + w
        else:
            self.pc += 1

    def tgl(self, r):
        t = self.pc + self.r[r]
        if t < len(self.prog):
            instr = self.prog[t]
            if len(instr) == 2:
                if instr[0] == 'inc':
                    self.prog[t][0] = 'dec'
                else:
                    self.prog[t][0] = 'inc'
            elif len(instr) == 3:
                if instr[0] == 'jnz':
                    self.prog[t][0] = 'cpy'
                else:
                    self.prog[t][0] = 'jnz'
        self.pc += 1

    def run(self):
        instr = self.prog[self.pc]
        if self.verbose: print instr
        eval('self.'+instr[0])(*instr[1:])
        if self.verbose: print self

    def __repr__(self):
        return repr((self.pc, dict(self.r)))

def run(overrides={}, verbose=False):
    r = defaultdict(int)
    for k, v in overrides.items():
        r[k] = v
    with open(sys.argv[1], 'r') as f:
        state = CPU(map(string.split, f.read().splitlines()), r)

    state.verbose = verbose

    while not state.done():
        state.run()
    return state.r['a']

print 'Part 1:', run(overrides={'a': 7})
print 'Part 2:', run(overrides={'a': 12}, verbose=True)
