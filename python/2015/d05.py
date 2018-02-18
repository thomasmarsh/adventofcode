import re
import sys

dupe = re.compile(r'((\w)\2)')
repeat = re.compile(r'((\w).\2)')

def nice(s):
    bad = ['ab', 'cd', 'pq', 'xy']
    for b in bad:
        if s.find(b) != -1:
            return False

    if not dupe.search(s):
        return False

    vowels = 'aeiou'
    count = 0
    for c in vowels:
        count += s.count(c)
        if count >= 3:
            return True
    return False

def nice2(s):
    found = False
    for i in range(len(s)):
        for j in range(i+2, len(s)-1):
            if s[i:i+2] == s[j:j+2]:
                found = True
                break
    if found:
        if repeat.search(s):
            return True
    return False


assert(nice('ugknbfddgicrmopn') == True)
assert(nice('aaa') == True)
assert(nice('jchzalrnumimnmhp') == False)
assert(nice('haegwjzuvuyypxyu') == False)
assert(nice('dvszwmarrgswjxmb') == False)

assert(nice2('qjhvhtzxzqqjkmpb') == True)
assert(nice2('xxyxx') == True)
assert(nice2('uurcxstgmygtbstg') == False)
assert(nice2('ieodomkazucvgmuy') == False)

with open(sys.argv[1], 'r') as f:
    lines = f.read().splitlines()

print 'Part 1:', reduce(lambda x, y: nice(y)+x, lines, 0)
print 'Part 2:', reduce(lambda x, y: nice2(y)+x, lines, 0)
