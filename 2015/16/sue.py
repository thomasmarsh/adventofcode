import re
import sys

PROFILE = {
    'children': '3',
    'cats': '7',
    'samoyeds': '2',
    'pomeranians': '3',
    'akitas': '0',
    'vizslas': '0',
    'goldfish': '5',
    'trees': '3',
    'cars': '2',
    'perfumes': '1',
}

def parse(line):
    sue = re.findall(r'Sue (\d+):', line)[0]
    i = line.find(':')
    parts = line[i+1:].split(',')
    d = {a.strip(): b.strip() for a, b in [x.split(':') for x in parts]}
    return sue, d


def load():
    with open(sys.argv[1], 'r') as f:
        lines = f.read().splitlines()
        return [parse(line) for line in lines]

def matches(entry, ranges):
    count = 0
    for k in PROFILE.keys():
        if entry.has_key(k):
            if ranges and k in ('cats', 'trees'):
                if entry[k] <= PROFILE[k]:
                    return False
            elif ranges and k in ('pomeranians', 'goldfish'):
                if entry[k] >= PROFILE[k]:
                    return False
            elif entry[k] != PROFILE[k]:
                return False
            count += 1
    if count > 0:
        return True
    return False

def search(ranges=False):
    data = load()
    for sue, entry in data:
        if matches(entry, ranges):
            return sue

print 'Part 1:', search()
print 'Part 2:', search(True)
