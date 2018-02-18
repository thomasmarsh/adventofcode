from sets import Set
import re

with open('input', 'r') as f:
    data = [l.strip() for l in f.readlines()]

def is_aba(s):
    assert(len(s) == 3)
    return s[0] == s[2] and s[0] != s[1]

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

def get_aba(s):
    aba = Set()
    for c in chunks(s, 2):
        if len(c) == 3:
            if is_aba(c):
                aba.add(c)
    return aba

def all_aba(l):
    aba = Set()
    for s in l:
        aba |= get_aba(s)
    return aba

def invert_aba(s):
    r = Set()
    for c in s:
        assert(len(c) == 3)
        r.add(c[1]+c[0]+c[1])
    return r


def support_ssl(addr):
    clean, bracketed = split_addr(addr)
    intersection = invert_aba(all_aba(clean)) & all_aba(bracketed)
    return intersection

c = 0
for d in data:
    if support_ssl(d):
        c+= 1

print c
