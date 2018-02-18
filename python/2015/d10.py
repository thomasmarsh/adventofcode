import re

def say(s):
    o = []
    for r in re.findall(r'((\d)\2+)|(\d)', s):
        if len(r[0]) > 0:
            o.append(str(len(r[0])) + r[0][0])
        else:
            o.append('1' + r[2])
    return ''.join(o)

def rsay(s, n):
    return reduce(lambda a, b: say(a), range(n), s)

assert(say('1') == '11')
assert(say('11') == '21')
assert(say('21') == '1211')
assert(say('1211') == '111221')
assert(say('111221') == '312211')
assert(rsay('1', 5) == '312211')

print 'Part 1:', len(rsay('1321131112', 40))
print 'Part 2:', len(rsay('1321131112', 50))
