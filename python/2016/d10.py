from collections import defaultdict
from collections import deque
import sys

bot = defaultdict(list)
output = defaultdict(list)
instructions = defaultdict(deque)

def parse(cmd):
    if cmd[0] == 'value':
        bot[cmd[-1]].append(int(cmd[1]))
    else:
        assert(cmd[0] == 'bot')
        low = cmd[5], cmd[6]
        high = cmd[10], cmd[11]
        instr = (low, high)
        instructions[cmd[1]].append(instr)

def perform(b):
    instr = instructions[b].popleft()

    low, high = sorted(bot[b])
    if low == 17 and high == 61:
        print '17/61 responsible:', b

    ((da, ai), (db, bi)) = instr
    eval(da)[ai].append(low)
    eval(db)[bi].append(high)

def run():
    ready = [k for k in bot.keys() if len(bot[k]) == 2]
    while ready:
        perform(ready[0])
        ready = [k for k in bot.keys() if len(bot[k]) == 2 and len(instructions[k])]
    a, b, c = [output[str(i)][-1] for i in [0, 1, 2]]
    print a*b*c

with open(sys.argv[1], 'r') as f:
    [parse(x.split()) for x in f.read().splitlines()]

run()
