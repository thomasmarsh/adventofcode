def next_tile(p):
    if p in ['^^.', '.^^', '^..', '..^']:
        return '^'
    return '.'

def next(pat):
    pat = '.' + pat + '.'
    x = [pat[i:i+3] for i in range(0, len(pat)-2)]
    return ''.join(map(next_tile, x))

def count_safe(pat, n):
    count = pat.count('.')
    for i in range(n-1):
        pat = next(pat)
        count += pat.count('.')
    return count

pat = '.^^^^^.^^.^^^.^...^..^^.^.^..^^^^^^^^^^..^...^^.^..^^^^..^^^^...^.^.^^^^^^^^....^..^^^^^^.^^^.^^^.^^'
print 'Part 1:', count_safe(pat, 40)
print 'Part 2:', count_safe(pat, 400000)
