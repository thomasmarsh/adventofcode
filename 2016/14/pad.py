import re
import md5
from hashlib import md5
from collections import deque

class LRUCache:
    def __init__(self, capacity):
        self.capacity = capacity
        self.tm = 0
        self.cache = {}
        self.lru = {}

    def get(self, key):
        if key in self.cache:
            self.lru[key] = self.tm
            self.tm += 1
            return self.cache[key]
	return None

    def set(self, key, value):
        if len(self.cache) >= self.capacity:
            old_key = min(self.lru.keys(), key=lambda k:self.lru[k])
            self.cache.pop(old_key)
            self.lru.pop(old_key)
        self.cache[key] = value
        self.lru[key] = self.tm
        self.tm += 1

def digest(salt, n, stretch):
    d = md5(salt + str(n)).hexdigest()
    for i in range(stretch):
        d = md5(d).hexdigest()
    return d

triple = re.compile(r'((\w)\2\2)')

def get(cache, key):
    d = cache.get(key)
    if not d:
        d = digest(*key)
        cache.set(key, d)
    return d

def scan(cache, salt, i, c, stretch):
    s = c*5
    for i in range(i+1, i+1001):
        d = get(cache, (salt, i, stretch))
        result = d.find(s)
        if result != -1:
            #print d, s
            return True
    return False


def pad_index(salt, stretch=0):
    cache = LRUCache(1001)

    i, count = 0, 0
    while count < 64:
        d = get(cache, (salt, i, stretch))
        #print i, d
        t = triple.findall(d)
        if t:
            if scan(cache, salt, i, t[0][1], 2016):
                print i, d, t[0][0]
                count += 1
        i += 1
    return i-1

assert(digest('test', 10, 0) != digest('test', 10, 2016))

#print pad_index('abc', 2016)
print pad_index('zpqevtbw', 2016)
