def load():
    with open('input') as f:
        return map(lambda x: x.strip(), f.readlines())

class Room:
    def __init__(self, s):
        tmp, checksum = s.split('[')
        parts = tmp.split('-')
        assert(checksum[-1] == ']')
        self.checksum = checksum[:-1]
        self.sector_id = int(parts[-1])
        self.name = '-'.join(parts[:-1])

    def __repr__(self):
        return '{}-{}[{}] {}'.format(self.name,
                                     self.sector_id,
                                     self.checksum,
                                     self.histogram())

    def histogram(self):
        unique = set(list(self.name))
        d = {letter: self.name.count(letter) for letter in unique}
        del d['-']
        return d

    def sorted_hist(self):
        def cmp((a, i), (b, j)):
            if i != j:
                return j - i
            return ord(a) - ord(b)
        return sorted(self.histogram().items(), cmp)

    def rc(self):
        return ''.join([c for (c,n) in self.sorted_hist()[:5]])

    def valid(self):
        return self.rc() == self.checksum

    def decrypt(self):
        def translate(c):
            if c == '-': return ' '
            x = ord(c) - ord('a')
            return chr((x + self.sector_id) % 26 + ord('a'))
        return ''.join([translate(c) for c in self.name])

def sector_if_valid(room):
    r = Room(room)
    if r.valid():
        return r.sector_id
    return 0

def part1(data):
    n = 0
    for line in data:
        n += sector_if_valid(line)
    return n

def part2(data):
    for line in data:
        r = Room(line)
        if r.valid():
            if r.decrypt().split()[0] == 'northpole':
                return r.sector_id
    return None

data = load()
print 'Part 1:', part1(data)
print 'Part 2:', part2(data)
