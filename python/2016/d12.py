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
        self.r[y] = int(x) if is_int(x) else self.r[x]
        self.pc += 1

    def inc(self, x):
        self.r[x] += 1
        self.pc += 1

    def dec(self, x):
        self.r[x] -= 1
        self.pc += 1

    def jnz(self, x, y):
        q = int(x) if is_int(x) else self.r[x]
        if q != 0:
            self.pc = self.pc + int(y)
        else:
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

    print 'initial:', state
    while not state.done():
        state.run()
    print state.r['a']

run()
run(overrides={'c': 1}, verbose=False)
