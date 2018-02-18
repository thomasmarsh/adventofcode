import sys
import re

test = [
    ('Comet', 14, 10, 127),
    ('Dancer', 16, 11, 162),
]

r = r'(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.'

def load():
    with open(sys.argv[1], 'r') as f:
        lines = f.read().splitlines()
    data = []
    for line in lines:
        row = re.findall(r, line)[0]
        data.append([row[0]] + [int(x) for x in row[1:]])
    print data
    return data

def step(racers, state):
    best = 0
    for i in range(len(racers)):
        name, speed, fly_dur, rest_dur = racers[i]
        moving, distance, rest_time, fly_time, score = state[i]
        if moving:
            fly_time += 1
            distance += speed
            if fly_time >= fly_dur:
                moving = 0
                rest_time = 0
        else:
            rest_time += 1
            if rest_time >= rest_dur:
                moving = 1
                fly_time = 0

        state[i] = moving, distance, rest_time, fly_time, score
        if distance > best:
            best = distance

    for i in range(len(racers)):
        if state[i][1] == best:
            tmp = list(state[i])
            tmp[-1] += 1
            state[i] = tuple(tmp)

    return state

def race(racers, tmax):
    state = [(1, 0, 0, 0, 0)]*len(racers)
    for t in range(tmax):
        state = step(racers, state)
    return state

def winning_dist(racers, n):
    return sorted([d for (_, d, _, _, _) in race(racers, n)])[-1]

def winning_score(racers, n):
    return sorted([d for (_, _, _, _, d) in race(racers, n)])[-1]

data = load()
print 'Part 1:', winning_dist(data, 2503)
print 'Part 2:', winning_score(data, 2503)
