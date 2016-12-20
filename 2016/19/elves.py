import math

# This is the Josephus problem: https://en.wikipedia.org/wiki/Josephus_problem
def josephus(n):
    return int(bin(n)[3:] + '1', 2)

def josephus3(n):
    p = 3 ** int(math.log(n-1, 3))
    return n - p+max(n-2*p, 0)

print 'Part 1:', josephus(3005290)
print 'Part 2:', josephus3(3005290)
