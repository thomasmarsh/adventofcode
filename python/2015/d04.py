from hashlib import md5

def hash(secret, n):
    return md5(secret + str(n)).hexdigest()

def search(secret, m):
    n = 0
    while True:
        h = hash(secret, n)
        if h.startswith('0'*m):
            return n
        n += 1

#assert(search('abcdef') == 609043)
print 'Part 1:', search('ckczppom', 5)
print 'Part 2:', search('ckczppom', 6)
