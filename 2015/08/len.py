import re

test = [
        '""',
        '"abc"',
        r'"aaa\"aaa"',
        r'"\x27"'
]

def load():
    with open('input', 'r') as f:
        return f.read().splitlines()

def count(line):
    return len(line), len(eval(line)), len(re.escape(line))+2

def search(data):
    sa = sb = sc = 0
    for line in data:
        a, b, c = count(line)
        sa += a
        sb += b
        sc += c
    return sa - sb, sc - sa

data = load()
print 'Part 1:', search(data)[0]
print 'Part 2:', search(data)[1]
