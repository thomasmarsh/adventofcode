import re

with open('input', 'r') as f:
    data = [l.strip() for l in f.readlines()]

def is_abba(s):
    assert(len(s) == 4)
    return s[0] == s[3] and s[1] == s[2] and s[0] != s[1]

def chunks(l, n):
    for i in xrange(0, len(l)):
        yield l[i:i+1+n]

def split_addr(s):
    result = [[], []]
    bracket = False
    buf = []
    def finalize():
        if buf:
            final = ''.join(buf)
            if bracket:
                result[1].append(final)
            else: result[0].append(final)
    for c in s:
        if c == '[':
            finalize()
            bracket = True
            buf = []
        elif c == ']':
            finalize()
            bracket = False
            buf = []
        else: buf.append(c)
    finalize()
    return result

def has_abba(s):
    for c in chunks(s, 3):
        if len(c) == 4:
            if is_abba(c):
                return True
    return False

def any_has_abba(l):
    for s in l:
        if has_abba(s):
            return True
    return False

def support_tls(addr):
    clean, bracketed = split_addr(addr)
    return any_has_abba(clean) and not any_has_abba(bracketed)

c = 0
for d in data:
    if support_tls(d):
        c+= 1

print c
