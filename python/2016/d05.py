import md5

test = 'abc'
data = 'cxdnnyjw'

prefix = data

def part1(prefix):
    i = 0
    p = ''
    for n in range(8):
        while True:
            x = prefix + str(i)
            i += 1
            digest = md5.md5(x).hexdigest()
            if digest.startswith('00000'):
                p += digest[5]
                break
    return p

def part2(prefix):
    i = 0
    p = ['_' for _ in range(8)]
    while p.count('_') != 0:
        x = prefix + str(i)
        i += 1
        digest = md5.md5(x).hexdigest()
        if digest.startswith('00000'):
            pos = digest[5]
            value = digest[6]
            if pos.isdigit() and int(pos) < len(p) and p[int(pos)] == '_':
                p[int(pos)] = value
                #print ''.join(p)
    return ''.join(p)

print 'Part 1:', part1(prefix)
print 'Part 2:', part2(prefix)
