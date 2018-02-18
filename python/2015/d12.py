import json
import sys

def jsum(j, ignore_red=False):
    def count(data, ignore_red):
        if type(data) == type(0):
            return data
        if type(data) == type([]):
            return reduce(lambda a, b: count(b, ignore_red)+a, data, 0)
        if type(data) == type({}):
            if ignore_red:
                if 'red' in data.values():
                    return 0
            return count([data[k] for k in data.keys()], ignore_red)
        return 0

    return count(json.loads(j), ignore_red)

assert(jsum('[[[3]]]') == 3) 
assert(jsum('[1,2,3]') == 6)
assert(jsum('{"a":2,"b":4}') == 6)
assert(jsum('[[[3]]]') == 3) 
assert(jsum('{"a":{"b":4},"c":-1}') == 3)
assert(jsum('{"a":[-1,1]}') == 0)
assert(jsum('[-1,{"a":1}]') == 0)
assert(jsum('[]') == 0)
assert(jsum('{}') == 0)

data = open(sys.argv[1], 'r').read()
print 'Part 1:', jsum(data)
print 'Part 2:', jsum(data, True)
