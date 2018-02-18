import math

def factors(n):
    step = 2 if n%2 else 1
    return set(reduce(list.__add__,
        ([i, n//i] for i in range(1, int(math.sqrt(n))+1, step) if n % i == 0)))

def search(n):
    part1, part2 = None, None
    i = 0
    while not (part1 and part2):
        i += 1
        divisors = factors(i)
        if not part1:
            if sum(divisors) * 10 >= n:
                part1 = i
        if not part2:
            if sum(d for d in divisors if i / d <= 50) * 11 >= n:
                part2 = i
    return part1, part2

part1, part2 = search(34000000)
print 'Part 1:', part1
print 'Part 2:', part2
