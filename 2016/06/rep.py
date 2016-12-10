with open('input') as f:
    data = [x.strip() for x in f.readlines()]

r = []
for i in range(len(data[0])):
    d = {}
    for entry in data:
        c = entry[i]
        if not d.has_key(c): d[c] = 0
        d[c] += 1
    r.append(d)

def freq(a,b):
    return cmp(a[1], b[1])

print ''.join([sorted(x.items(), cmp=freq)[0][0] for x in r])
