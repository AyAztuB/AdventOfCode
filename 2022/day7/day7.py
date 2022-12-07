import os
from collections import defaultdict

os.system("aoc d -d 7 -y 2022 -I -i input.txt -o")

def submition(answer, part):
    print(answer)
    os.system(f"aoc s {part} {answer} -d 7 -y 2022")

def load_data():
    res = []
    with open("input.txt", "r") as f:
        res.extend(l.replace('\n', '') for l in f if len(l) > 1)
    return res

def __compute_size():
    data = load_data()
    res = defaultdict(int)
    path = []
    for l in data:
        c = l.strip().split()
        if c[1] == 'cd' and c[2] == '..':
            path.pop()
        elif c[1] == 'cd':
            path.append(c[2])
        elif c[1] == 'ls' or c[0] == 'dir':
            continue
        else:
            size = int(c[0])
            for i in range(1, len(path)+1):
                res['/'.join(path[:i])] += size
    return res

def part1():
    sizes = __compute_size()
    return sum(s for s in sizes.values() if s <= 100000)

submition(part1(), 1)

def part2():
    sizes = __compute_size()
    _max = 70000000 - 30000000
    _total = sizes['/']
    _to_delete = _total - _max
    res = 1e15
    for v in sizes.values():
        if v >= _to_delete:
            res = min(res, v)
    return res

submition(part2(), 2)


