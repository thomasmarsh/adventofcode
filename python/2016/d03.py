def load1():
    with open('input') as f:
        return map(lambda x: map(lambda y: int(y),
                                 x.strip().split()),
                   f.readlines())

def load2():
    loaded = []
    with open('input') as f:
        buf = [[],[],[]]
        for line in f.readlines():
            data = map(lambda y: int(y), line.strip().split())
            for i in range(3):
                buf[i].append(data[i])
            if len(buf[0]) >= 3:
                map(loaded.append, buf)
                buf = [[], [], []]
        assert(len(buf[0]) == 0)
    return loaded


def tcount(data):
    count = 0
    for entry in data:
        entry.sort()
        a,b,c = entry
        if a+b > c:
            count += 1
    print count

def main():
    tcount(load1())
    tcount(load2())

if __name__ == '__main__': main()
