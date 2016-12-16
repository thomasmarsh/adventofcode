def r(n):
    return ''.join([str(x) for x in n])

def data(a, n):
    while len(a) < n:
        b = [1-int(x) for x in a]
        b.reverse()
        a += '0' + r(b)
    return a[:n]

def checksum(s):
    while len(s) % 2 == 0:
        m = { '00': '1', '11': '1', '01': '0', '10': '0' }
        c = []
        for i in range(0, len(s), 2):
            c.append(m[s[i:i+2]])
        s = ''.join(c)
    return s

assert(data('1', 3) == '100')
assert(data('0', 3) == '001')
assert(data('11111', 11) == '11111000000')
assert(data('111100001010', 25) == '1111000010100101011110000')
assert(checksum('110010110100') == '100')
assert(checksum(data('10000', 20)) == '01100')

print 'Part 1:', checksum(data('10111011111001111', 272))
