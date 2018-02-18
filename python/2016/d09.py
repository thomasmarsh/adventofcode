import re

v1_data = [
        ("ADVENT", len("ADVENT")),
        ("A(1x5)BC", len("ABBBBBC")),
        ("(3x3)XYZ", len("XYZXYZXYZ")),
        ("A(2x2)BCD(2x2)EFG", len("ABCBCDEFEFG")),
        ("(6x1)(1x3)A", len("(1x3)A")),
        ("X(8x2)(3x3)ABCY", len("X(3x3)ABC(3x3)ABCY"))
]

v2_data = [
        ('(3x3)XYZ', 9),
        ('X(8x2)(3x3)ABCY', 20),
        ('(27x12)(20x12)(13x14)(7x10)(1x12)A', 241920),
        ('(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN', 445),
]

pattern = re.compile(r'\((\d+)x(\d+)\)')

def count(s, v2=False):
    m = pattern.search(s)
    if not m:
        return len(s)

    # head is pure uncompressed data
    head = s[:m.start()]

    # a is the length of string affected
    # b is the number of repetitions
    (a,b) = [int(m.group(i)) for i in (1,2)]

    # work includes everything covered by the decompression marker
    work = s[m.end():m.end()+a]

    # tail is the unprocessed remained
    tail = s[m.end()+a:]

    n = 0

    if v2:
        n += count(work, v2) * b
    else:
        n += len(work) * b

    return len(head) + n + count(tail, v2)

def test(data, v2):
    for input, output in data:
        x = count(input, v2)
        print 'f(', input, ') =', output, '[', x, ']'
        assert(x == output)


test(v1_data, False)
print '--'
test(v2_data, True)
print '--'
data = open('input', 'r').read().strip()
print 'Part 1:', count(data, False)
print 'Part 2:', count(data, True)
