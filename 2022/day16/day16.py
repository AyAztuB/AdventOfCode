from os import system
import itertools
import re

system("aoc d -d 16 -y 2022 -I -i 16.in -o")

def submit(answer, part):
    system(f"echo {answer} && aoc s {part} {answer} -d 16 -y 2022")

def load_data():
    data = []
    with open("16.in", "r") as f:
        data.extend(l.replace('\n', '') for l in f if len(l) > 1)
    return data

def __prepare_data(data):
    edges = {}
    rates = {}
    RATE = re.compile(r'rate=(\d+);')
    VALVES = re.compile(r'to valves? (.*)$')
    for l in data:
        _, name, *_ = l.split()
        tunnels = VALVES.search(l)[1].split(', ')
        rate = int(RATE.search(l)[1])
        edges[name] = tunnels
        rates[name] = rate
    w = {}
    good_rates = {name for name, rate in rates.items() if rate}
    main_edges = ['AA', *good_rates]
    for a, b in itertools.combinations(main_edges, r=2):
        __e = [(a,)]
        while __e:
            __p = __e.pop(0)
            if __p[-1] == b:
                break
            else:
                __e.extend((*__p, n) for n in edges[__p[-1]])
        w[(a, b)] = len(__p)
        w[(b, a)] = len(__p)
    return (rates, good_rates, w)

def part1(data):
    rates, good_rates, w = __prepare_data(data)
    best = -1
    __todo = [(0, 0, ('AA',), good_rates)]
    while __todo:
        score, time, path, tunnels = __todo.pop()
        best = max(best, score)
        for __p in tunnels:
            __time = time+w[(path[-1], __p)]
            if __time < 30:
                __todo.append((
                    score + (30 - __time) * rates[__p],
                    __time,
                    path + (__p,),
                    {e for e in tunnels if e != __p},
                ))
    return best

# submit(part1(load_data()), 1)


def part2(data):
    rates, good_rates, w = __prepare_data(data)
    best = {}
    __todo = [(0, 0, ('AA',), good_rates)]
    while __todo:
        score, time, path, tunnels = __todo.pop()
        __path = frozenset(path) - {'AA'}
        __best = best.setdefault(__path, score)
        best[__path] = max(__best, score)
        for __p in tunnels:
            __time = time + w[(path[-1], __p)]
            if __time < 26:
                __todo.append((
                    score + (26 - __time) * rates[__p],
                    __time,
                    path + (__p,),
                    {e for e in tunnels if e != __p},
                ))
    return max(best[__1] + best[__2] for __1, __2 in itertools.combinations(best, r=2) if not __1 & __2)

# submit(part2(load_data()), 2)

def __test(f, exp):
    data = ("Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n"
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA\n"
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB\n"
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n"
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD\n"
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG\n"
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH\n"
    "Valve HH has flow rate=22; tunnel leads to valve GG\n"
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ\n"
    "Valve JJ has flow rate=21; tunnel leads to valve II\n")
    __data = [l.replace('\n', '') for l in data.split('\n') if len(l) > 1]
    res = f(__data)
    print(res)
    print(res == exp)

# __test(part1, 1651)
# __test(part2, 1707)




