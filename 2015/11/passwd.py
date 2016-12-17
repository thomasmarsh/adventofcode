import re

LEN = 8

def p2n(p):
    i = 0
    n = 0
    for c in reversed(p):
        n += (ord(c)-ord('a')) * (26 ** i)
        i += 1
    return n

def n2p(n):
    cs = []
    while n > 0:
        cs.append(chr((n % 26) + ord('a')))
        n /= 26
    p = ''.join(reversed(cs))
    return 'a'*(LEN-len(p)) + p

def straight(p):
    ns = [p2n(n) for n in p]
    for i in range(len(ns)-2):
        a,b,c = ns[i:i+3]
        if b == a+1 and c == b+1:
            return True
    return False

def valid_chars(p):
    return re.search('[iol]', p) == None

def has_pairs(p):
    r = re.compile(r'((\w)\2)')

    first = r.search(p)
    if not first:
        return False

    substr = p[first.span()[1]:]
    while True:
        second = r.search(substr)
        if not second:
            return False
        if first.groups(0) != second.groups(0):
            return True
        substr = substr[second.span()[1]:]

def valid(p):
    return len(p) == LEN \
       and valid_chars(p) \
       and straight(p) \
       and has_pairs(p)

def next(p):
    return n2p(p2n(p)+1)

def next_valid(p):
    assert(len(p) == LEN)
    while True:
        m = re.search('[iol]', p)
        if m:
            c = chr(ord(m.group(0))+1)
            p = p[:m.span()[0]] + c + 'a'*(LEN-m.span()[1])
            if valid(p):
                return p
        p = next(p)
        if valid(p):
            return p


assert(not valid('hijklmmn'))
assert(not valid('abbceffg'))
assert(next_valid('abcdefgh') == 'abcdffaa')
assert(next_valid('ghijklmn') == 'ghjaabcc')

p = next_valid('cqjxjnds')
print 'Part 1:', p
print 'Part 2:', next_valid(p)
